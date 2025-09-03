// Practice with a single number
const num = 12;

/*
Ignore any lines that look like the following.
They are here so you can see the exercise numbers in the output.
*/
process.stdout.write('*4·1. ');

/*
*4·1. Only print the following message if num is greater than 10.
Try changing the value of num to make sure your code works.
*/
if (num > 10) {
  console.log('num is greater than 10');
}


process.stdout.write('\n*4·11. ');
/*
*4·11. Only print the message 'num is less than 100' when appropriate.
*/
if (num < 100) {
  console.log('num is less than 100');
}


process.stdout.write('\n*4·12. ');
/*
*4·12. Now do the same with 'num is a positive number'.
*/
if (num > 0) {
  console.log('num is a positive number')
}


process.stdout.write('\n*4·13. ');
/*
*4·13. 'num is between 10 and 100' (exclusive)
*/
if (num > 10 && num < 100) {
  console.log('num is between 10 and 100');
}


process.stdout.write('\n*4·14. ');
/*
*4·14. 'num is even'
*/
if (num % 2 === 0) {
  console.log('num is even');
}


process.stdout.write('\n*4·15. ');
/*
*4·15. 'num is an even number between 20 and 30' (inclusive)
*/
if (num % 2 === 0 && num >= 20 && num <= 30) {
  console.log('num is an even number between 20 and 30');
}


process.stdout.write('\n*4·16. ');
/*
*4·16. 'num is odd or negative'
*/
if (num % 2 !== 0 || num < 0) {
  console.log('num is odd or negative');
}


// Practice comparing numbers
const x = 3;
const y = 5;

process.stdout.write('\n*4·2. ');
process.stdout.write('The bigger number is: ');
/*
*4·2. Print either x or y, whichever is bigger.
*/
if (x > y) {
  console.log(x);
} else {
  console.log(y);
}


process.stdout.write('\n*4·21. ');
/*
*4·21. Print either the message 'x and y are equal' or 'x and y are not equal'.
*/
if (x === y) {
  console.log('x and y are equal');
} else {
  console.log('x and y are not equal');
}


// Let's add another number into the mix.
const z = 2;

process.stdout.write('\n*4·3. ');
process.stdout.write('The biggest number is: ');
/*
*4·3. Print either x, y, or z, whichever is bigger.
Remember to change the values of x, y, and z to make sure it works!
*/
if (x > y && x > z) {
  console.log(x);
} else if (y > x && y > z) {
  console.log(y);
} else {
  console.log(z);
}


// Practice with more operators
const walletInCents = 25;
const priceInDollars = 1.25;

process.stdout.write('\n*4·4. ');
/*
*4·4. Print 'Exact amount!' when the wallet contains the exact amount for the price. Print 'More than enough.' when there is more than enough money in the wallet. Print 'Not enough money.' when there is not enough money in the wallet.

Notice that the wallet is in cents and the price is in dollars, so you'll have to do a bit of work to compare them.
*/
if (walletInCents === priceInDollars * 100) {
  console.log('Exact amount!');
} else if (walletInCents > priceInDollars * 100) {
  console.log('More than enough!');
} else {
  console.log('Not enough money.');
}


process.stdout.write('\n*4·41. ');
/*
*4·41. Do the same thing as in 11, but let's add in some error handling. In the case where the wallet is negative, print 'Wallet cannot be negative.' When the price is negative, print 'Price cannot be negative.' Think about the importance of the order of the conditions.
*/
if (walletInCents < 0) {
  console.log('Wallet cannot be negative.');
} else if (priceInDollars < 0) {
  console.log('Price cannot be negative.');
} else if (walletInCents === priceInDollars * 100) {
  console.log('Exact amount!');
} else if (walletInCents > priceInDollars * 100) {
  console.log('More than enough!');
} else {
  console.log('Not enough money.');
}


// Practice comparing strings
let username = 'Alice';
let password = '1234';
const expectedUsername = 'Zed';
const expectedPassword = '9876';

process.stdout.write('\n*4·5. ');
/*
*4·5. To simulate authentication, we have a username, a password, and expected values for each. The login is successful if the username and password match the expected values. Otherwise, either the username is wrong or the password is wrong. Print appropriate messages for each of the 3 cases. (Don't worry about the case where the username and password are both wrong; one of the other cases will catch it.)
*/
if (username === expectedUsername && password === expectedPassword) {
  console.log('Login successful!');
} else if (username !== expectedUsername) {
  console.log('Wrong username.');
} else {
  console.log('Wrong password.')
}


// Practice nesting conditionals
const a = 1;
const b = 2;

process.stdout.write('\n*4·6. ');
/*
*4·6. Print the appropriate message for any of the following cases comparing a and b:

Matching positive numbers.
Matching negative numbers.
No match of positive numbers.
No match of negative numbers.
No match.
*/
if (a === b) {
  if (a > 0 && b > 0) {
    console.log('Matching positive numbers.');
  } else {
    console.log('Matching negative numbers.');
  }
} else {
  if (a > 0 && b > 0) {
    console.log('No match of positive numbers.');
  } else if (a < 0 && b < 0) {
    console.log('No match of negative numbers.');
  } else {
    console.log('No match.')
  }
}


// Practice with more information
const myUsername = 'Alice';
const myPassword = '1234';
const salary = 25000;

const employeeUsername = 'Zarya';
const employeePassword = '9876';
const ceoUsername = 'Xyrho';
const ceoPassword = 'hunter2';
const juniorSalaryMinimum = 50000;
const juniorSalaryMaximum = 70000;
const intermediateSalaryMinimum = 75000;
const intermediateSalaryMaximum = 90000;
const seniorSalaryMinimum = 95000;
const seniorSalaryMaximum = 120000;
const ceoSalaryMinimum = 110000;

process.stdout.write('\n*4·61. ');
/*
*4·61. We have another authentication, but this time with a salary as well. We want to print a personalized welcome message to users who log in.

For example, if the username and password match the employee username and password and their salary is within the junior range, print 'Welcome, junior employee Zarya.' In the case where the salary is not within any of the set ranges, simply print 'Welcome, employee Zarya.' If the username and password match the CEO credentials but the salary is below the CEO's minimum salary, we have an imposter! For an invalid login, just print 'Invalid login.' (don't worry about whether it was the username or password that didn't match).

If you notice you're repeating a certain condition more than once, try nesting if statements to avoid the repetition.
*/
if (myUsername === employeeUsername && myPassword === employeePassword) {
  if (salary >= juniorSalaryMinimum && salary <= juniorSalaryMaximum) {
    console.log('Welcome, junior employee Zarya.');
  } else if (salary >= intermediateSalaryMinimum && salary <= intermediateSalaryMaximum) {
    console.log('Welcome, intermediate employee Zarya.');
  } else if (salary >= seniorSalaryMinimum && salary <= seniorSalaryMaximum) {
    console.log('Welcome, senior employee Zarya.');
  } else {
    console.log('Welcome, employee Zarya.');
  }
} else if (myUsername === ceoUsername && password === ceoPassword) {
  if (salary >= ceoSalaryMinimum) {
    console.log('Welcome, CEO Xyrho.');
  } else {
    console.log('Imposter!');
  }
} else {
  console.log('Invalid login.')
}


// Practice complex conditions
const creditCard = '55560';

process.stdout.write('\n*4·7. ');
/*
*4·7. Print a message saying whether the credit card number is valid or not. A valid credit card in this system must have all of the following properties:

- It is exactly 5 digits and doesn't begin with any 0's
- Tripling it gives an even number
- It is divisible by either 5 or 7
*/
if ((creditCard >= 10000 && creditCard < 100000)
    && (creditCard * 3) % 2 === 0
    && (creditCard % 5 === 0 || creditCard % 7 === 0)) {
  console.log('Valid credit card!');
} else {
  console.log('Invalid credit card.');
}


process.stdout.write('\n');
