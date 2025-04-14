# DAP TUI

A basic terminal client for the [Debug Adapter Protocol](https://microsoft.github.io/debug-adapter-protocol).

## Status

Unstable. Work in progress.

## Development

The enviroment is declared in a [nix flake](https://nixos.wiki/wiki/flakes) file.

1. [Install `nix` on your system](https://nixos.org/download/)
2. Activate the environment `nix --experimental-features 'nix-command flakes' develop`
3. Run the application using `fennel ./dap-tui/main.fnl`

The application expects to find a running DAP server on port 5678.
For example, you may start [`debugpy`](https://github.com/microsoft/debugpy) using the following command:

    python -m debugpy --listen 5678 --wait-for-client <your-python-script.py>

Then start `dap-tui`.

## License

    Copyright 2025 msladecek

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
