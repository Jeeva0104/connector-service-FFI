"use strict";

const { authorize } = require("./index.js");

const req = {
  amount: 10,
  minor_amount: 10,
  payment_id: "987654321",
  confirm: true,
  merchant_account: "Hyperswitch",
  payment_method: {
    card: {
      card_number: 4111111111111111,
      card_cvc: 123,
      card_issuer: null
    }
  }
};

try {
  const res = authorize(req);
  console.log("Authorize response:");
  console.dir(res, { depth: null });
} catch (err) {
  console.error("Authorize failed:", err);
  process.exit(1);
}
