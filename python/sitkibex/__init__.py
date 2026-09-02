#
#  Copyright Bradley Lowekamp
#
#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
#
#         http://www.apache.org/licenses/LICENSE-2.0.txt
#
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.
#
# Vendored under python/sitkibex/ (Apache-2.0). See THIRD_PARTY.md — upstream is
# https://github.com/niaid/sitk-ibex (v0.2.1, Zenodo 4632320). The upstream shipped a
# CLI + OME-XML reader we don't use; only the registration/resample engine is vendored.
# The upstream __init__ looked its own version up via importlib.metadata — vendored
# copies have no dist-info, so we just pin the version we snapshotted.
from .registration import registration
from .resample import resample

__version__ = "0.2.1+cecelia"
__author__ = ["Bradley Lowekamp"]
__all__ = ["registration", "resample"]
