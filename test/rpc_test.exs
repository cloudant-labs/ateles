defmodule RpcTest do
  use ExUnit.Case

  setup_all do
    :ateles_rpc.init()
  end

  test "creates context" do
    {resp, _, _} = :ateles_rpc.create_context(%{:context_id => "context1"})
    assert resp == :ok

    {resp, _} = :ateles_rpc.create_context(%{:context_id => "context1"})
    assert resp == :error
  end

  test "can map" do
    context_id = "map_context"
    {resp, _, _} = :ateles_rpc.create_context(%{:context_id => context_id})
    assert resp == :ok

    {resp, _, _} =
      :ateles_rpc.add_map_funs(%{
        :context_id => context_id,
        :map_funs => [
          "function (doc) { emit(doc._id, doc.value)}",
          "function (doc) { emit([doc._id, doc.value], 1)}"
        ]
      })

    assert resp == :ok

    docs =
      for i <- 1..1 do
        %{
          :_id => "doc-id-#{i}",
          :value => i
        }
      end

    Enum.each(docs, fn doc ->
      {ok, stream} = :ateles_rpc.map_docs()
      IO.inspect doc
      opts = %{:context_id => context_id, :map_id => doc._id, :doc => :jiffy.encode(doc)}
      ok = :grpcbox_client.send(stream, opts)
      resp = :grpcbox_client.recv_data(stream)
      IO.inspect(resp)
      resp = :grpcbox_client.recv_data(stream)
      IO.inspect(resp)
    end)
  end
end
