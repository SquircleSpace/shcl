# Copyright 2019 Bradley Jensen
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

{}:
import ../default.nix {
  pkgs = import (builtins.fetchTarball {
    name = "nixos-19.03-2019-06-21";
    url = https://github.com/nixos/nixpkgs/archive/30a82bba734bc8d74fd291a0f7152809fb2cd037.tar.gz;
    sha256 = "1zf7svfmgfx64lpwf19a1q87rlqx6clwwfd1dir769n86hz5b0yr";
  }) {};
}
