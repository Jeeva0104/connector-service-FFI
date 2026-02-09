# connector-service-FFI

   Multi-language FFI bindings for the connector service. Supports Node.js (NAPI-RS), Python (UniFFI), and Haskell (C FFI via cbindgen).

   ## Prerequisites

   - **Rust** (latest stable)
   - **Node.js** & npm (for Node.js bindings)
   - **Python 3** (for Python bindings)
   - **GHC** & **cabal** (for Haskell bindings)
   - **uniffi-bindgen** (for Python): `cargo install uniffi-bindgen`
   - **cbindgen** (for Haskell): `cargo install cbindgen`

   ## Quick Start

   ### Node.js (NAPI-RS) - Default

   ```bash
   # Build
   npm run build

   # Test
   npm run test
   ```
   ## Build Commands

   | Target | Command | Output |
   |--------|---------|--------|
   | **Node.js** | `npm run build` | `index.node` |
   | **Python** | `npm run build:python` | `bindings/python/` |
   | **Haskell** | `npm run build:haskell` | `bindings/headers/connector_service.h` |
   | **Rust (dev)** | `cargo build` | `target/debug/` |
   | **Rust (release)** | `cargo build --release` | `target/release/` |

   ## Test Commands

   | Target | Command |
   |--------|---------|
   | **All** | `npm run test:all` |
   | **Node.js** | `npm run test` |
   | **Python** | `npm run test:python` |
   | **Haskell** | `npm run test:haskell` |
   | **Rust** | `cargo test` or `npm run rust:test` |

   ## Detailed Build Instructions

   ### Node.js (Default - NAPI-RS)

   Uses `napi-rs` to generate Node.js native addon.

   ```bash
   # Install dependencies
   npm install

   # Build (production)
   npm run build

   # Build (debug)
   npm run build:debug

   # Test
   npm run test
   ```


### Haskell (C FFI via cbindgen)
    Generates C headers with `cbindgen` and uses them in Haskell tests.
    
    ```bash
    # Generate C headers

    cbindgen --config cbindgen.toml --crate ffi --output bindings/headers/connector_service.h

    # Build Haskell tests

    cd tests/test_haskell && cabal run
    ```

### Python (UniFFI)
    Uses `uniffi-bindgen` to generate Python bindings.

    ```bash
    # Generate Python bindings

   cargo build --release --no-default-features --features uniffi && uniffi-bindgen generate --library target/release/libffi.dylib --language python --out-dir bindings/python && cp target/release/libffi.dylib bindings/python/

    # Install the generated package

    pip install -e bindings/python

    # Test

    python tests/test_python.py
    ```

## License

   MIT

   - ```bash


  ```

-
