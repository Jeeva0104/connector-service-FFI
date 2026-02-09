#!/usr/bin/env python3
"""
Python Test for connector-service-FFI (UniFFI bindings)

Run: python tests/test_python.py

Prerequisites:
  1. Build with UniFFI: cargo build --release --no-default-features --features uniffi
  2. Generate bindings: uniffi-bindgen generate --library target/release/libffi.dylib --language python --out-dir bindings/python
  3. Copy library: cp target/release/libffi.dylib bindings/python/
"""

import json
import sys

# Add bindings directory to path
sys.path.insert(0, "bindings/python")

from ffi import authorize_payment

test_request = {
    "amount": 1000,
    "minor_amount": 100,
    "payment_id": "pay_123",
    "confirm": True,
    "merchant_account": "merchant_1",
    "payment_method": {
        "card": {"card_number": 4111111111111111, "card_cvc": 123, "card_issuer": None}
    },
}


result = authorize_payment(json.dumps(test_request))
print(result)
