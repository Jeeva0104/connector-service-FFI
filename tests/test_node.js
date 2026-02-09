/**
 * Node.js Test for connector-service-FFI
 * 
 * Run: npm run test
 */

const { authorize } = require('../index');

const testRequest = {
    amount: 1000,
    minor_amount: 100,
    payment_id: "pay_123",
    confirm: true,
    merchant_account: "merchant_1",
    payment_method: {
        card: {
            card_number: 4111111111111111,
            card_cvc: 123,
            card_issuer: null
        }
    }
};

console.log("=== Node.js FFI Tests ===\n");

// Test 1: Valid authorization request
function testValidRequest() {
    try {
        const result = authorize(JSON.stringify(testRequest));
        
        // Handle both string and object responses
        let parsed;
        if (typeof result === 'string') {
            parsed = JSON.parse(result);
        } else {
            parsed = result; // Already an object
        }
        
        if (parsed.amount === 1000 && parsed.merchant_account === "merchant_1") {
            console.log("✓ Test 1 PASSED: Valid authorization request");
            console.log("  Response:", JSON.stringify(parsed));
            return true;
        } else {
            console.error("✗ Test 1 FAILED: Unexpected response structure");
            console.error("  Got:", result);
            return false;
        }
    } catch (e) {
        console.error("✗ Test 1 FAILED:", e.message);
        return false;
    }
}

// Test 2: Invalid JSON input
function testInvalidJson() {
    try {
        authorize("invalid json");
        console.error("✗ Test 2 FAILED: Should have thrown an error");
        return false;
    } catch (e) {
        console.log("✓ Test 2 PASSED: Invalid JSON correctly rejected");
        console.log("  Error:", e.message);
        return true;
    }
}

// Test 3: Empty input
function testEmptyInput() {
    try {
        authorize("");
        console.error("✗ Test 3 FAILED: Should have thrown an error");
        return false;
    } catch (e) {
        console.log("✓ Test 3 PASSED: Empty input correctly rejected");
        return true;
    }
}

// Test 4: Missing required fields
function testMissingFields() {
    try {
        authorize(JSON.stringify({ amount: 1000 }));
        console.error("✗ Test 4 FAILED: Should have thrown an error");
        return false;
    } catch (e) {
        console.log("✓ Test 4 PASSED: Missing fields correctly rejected");
        return true;
    }
}

// Run all tests
const results = [
    testValidRequest(),
    testInvalidJson(),
    testEmptyInput(),
    testMissingFields()
];

console.log("\n=== Test Summary ===");
const passed = results.filter(r => r).length;
const total = results.length;
console.log(`${passed}/${total} tests passed`);

if (passed !== total) {
    process.exit(1);
}