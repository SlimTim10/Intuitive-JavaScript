/*
16. Print a message saying whether the credit card number is valid or not. A valid credit card in this system must have all of the following properties:

- It is exactly 5 digits and doesn't begin with any 0's
- Tripling it gives an even number
- It is divisible by either 5 or 7
*/

const isCreditCardValid = (creditCard) => creditCard * 3;

if (isCreditCardValid(123)) {
  console.log('Valid credit card!');
} else {
  console.log('Invalid credit card.');
}

// isCreditCardValid(12345);
// isCreditCardValid(12340);
// isCreditCardValid(123406789);
