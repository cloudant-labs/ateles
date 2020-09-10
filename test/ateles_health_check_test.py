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

import requests
import unittest


class HealthChecker(object):

    def __init__(self, host, port):
        self.host = host
        self.port = port

    def get_result(self):
        addr = "{}:{}/Health".format(self.host, self.port)
        return requests.get("http://{}".format(addr))


class TestHealthCheck(unittest.TestCase):

    @classmethod
    def setUp(cls):
        cls.hc = HealthChecker("localhost", "50051")

    def test(self):
        r = self.hc.get_result()
        assert (r.status_code == 200)

if __name__ == '__main__':
    unittest.main()
