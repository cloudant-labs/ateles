# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy of
# the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.

defmodule RpcTest do
  use ExUnit.Case

  setup_all do
    :ateles_rpc.init()
  end

  test "creates context" do
    {resp, _, _} = :ateles_rpc.create_context(%{context_id: "context1"})
    assert resp == :ok

    {resp, _} = :ateles_rpc.create_context(%{context_id: "context1"})
    assert resp == :error
  end

  test "can map" do
    context_id = "map_context"
    {resp, _, _} = :ateles_rpc.create_context(%{context_id: context_id})
    assert resp == :ok

    map_funs = [
      %{
        id: "1",
        fun: "function (doc) { emit(doc._id, doc.value)}"
      },
      %{
        id: "2",
        fun: "function (doc) { emit([doc._id, doc.value], 1)}"
      }
    ]

    {resp, _, _} =
      :ateles_rpc.add_map_funs(%{
        context_id: context_id,
        map_funs: map_funs
      })


    assert resp == :ok

    docs =
      for i <- 1..3 do
        %{
          _id: "doc-id-#{i}",
          value: i
        }
      end

    {:ok, stream} = :ateles_rpc.map_docs()

    Enum.each(docs, fn doc ->
      opts = %{context_id: context_id, map_id: doc._id, doc: :jiffy.encode(doc)}
      :ok = :grpcbox_client.send(stream, opts)
      {:ok, %{result: result_str}} = :grpcbox_client.recv_data(stream)
      result = :jiffy.decode(result_str, [:return_maps])

      assert result == [
               %{
                 "id" => "1",
                 "result" => [[doc._id, doc.value]]
               },
               %{
                 "id" => "2",
                 "result" => [[[doc._id, doc.value], 1]]
               }
             ]

      resp = :grpcbox_client.recv_data(stream)
      assert resp == :timeout
    end)
  end
end
