// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations under
// the License.

#ifndef ATELES_LRU_H
#define ATELES_LRU_H

#include <list>
#include <stdexcept>
#include <unordered_map>

// This is a very basic LRU. The main thing to note
// is that a default constructed `value_type` is used
// as the sentinel value for "key not present". This
// works naturally for smart pointers but may not work
// well for other types.

namespace ateles
{
template <typename key_type, typename value_type>
class LRU {
  public:
    typedef std::list<key_type> key_list_type;
    typedef std::pair<value_type, typename key_list_type::iterator> entry_type;
    typedef std::unordered_map<key_type, entry_type> key_map_type;

    explicit LRU(size_t capacity) : _capacity(capacity) {}

    value_type get(key_type key)
    {
        auto iter = this->_key_map.find(key);
        if(iter != this->_key_map.end()) {
            // Found this splice gem here:
            //   https://timday.bitbucket.io/lru.html
            this->_key_list.splice(
                this->_key_list.end(), this->_key_list, (*iter).second.second);
            return iter->second.first;
        } else {
            return value_type();
        }
    }

    void put(key_type key, value_type value)
    {
        auto kiter = this->_key_map.find(key);
        if(kiter == this->_key_map.end()) {
            if(this->_key_map.size() >= this->_capacity) {
                this->evict();
            }

            auto liter = this->_key_list.insert(this->_key_list.end(), key);
            this->_key_map[key] = std::make_pair(value, liter);
        } else {
            this->_key_list.splice(
                this->_key_list.end(), this->_key_list, (*kiter).second.second);
        }
    }

    void clear()
    {
        this->_key_list.clear();
        this->_key_map.clear();
    }

  private:
    void evict()
    {
        assert(!this->_key_map.empty());
        assert(!this->_key_list.empty());

        auto iter = this->_key_map.find(this->_key_list.front());
        assert(iter != this->_key_map.end());

        this->_key_map.erase(iter);
        this->_key_list.pop_front();
    }

    size_t _capacity;
    key_list_type _key_list;
    key_map_type _key_map;
};

}  // namespace ateles

#endif  // included lru.h