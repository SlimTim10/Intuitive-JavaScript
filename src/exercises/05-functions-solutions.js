// Practice calling a function with no arguments

/*
5.1 Call the following function.
*/
const sayHello = () => {
  console.log('Hello, world!');
};

sayHello();

/*
5.11 Call the following function.
*/
const sayHelloLonger = () => {
  console.log('Hello!');
  console.log('Bonjour!');
  console.log('Guten Tag!');
};

sayHelloLonger();

// Practice creating a function with no arguments

/*
5.2 Create a function so the following line of code prints the message 'Welcome!'
*/
const sayWelcome = () => {
  console.log('Welcome!');
};
sayWelcome();

// Practice calling a function with one argument

/*
5.3 Call the following function, providing a language as the argument.
*/
const sayHelloLanguage = language => {
  if (language === 'English') {
    console.log('Hello!');
  } else if (language === 'French') {
    console.log('Bonjour!');
  } else if (language === 'German') {
    console.log('Guten Tag!');
  } else {
    console.log('Unknown language');
  }
};

sayHelloLanguage('German');

// Practice creating a function with one argument

/*
5.4 Create a function called 'greet' that takes a name as an argument. Call the function a few times to achieve the same goal as the following lines of code.
*/
console.log('Hello, Alice!');
console.log('Hello, Bob!');
console.log('Hello, Carol!');
console.log('Hello, Dean!');

const greet = name => {
  console.log('Hello, ' + name + '!');
};

greet('Alice');
greet('Bob');
greet('Carol');
greet('Dean');

/*
5.41 Create a function called 'isPositive' that takes a number as an argument and returns true if the number is positive (greater than 0), otherwise false.

The following lines should help test if your function works correctly. They should print true.
*/
const isPositive = x => x > 0;
console.log('10 is positive:', isPositive(10) === true);
console.log('-10 is not positive:', isPositive(-10) === false);

/*
5.411 Create a function called 'isNegative' that takes a number as an argument and returns true if the number is negative (less than 0), otherwise false.

The following lines should help test if your function works correctly.
*/
const isNegative = x => x < 0;
console.log('10 is not negative:', isNegative(10) === false);
console.log('-10 is negative:', isNegative(-10) === true);

/*
5.42 Create a function called 'isEven' that takes a number as an argument and returns true if the number is even, otherwise false.

Write some lines of code to test if your function works correctly.
*/
const isEven = x => x % 2 === 0;
console.log('2 is even:', isEven(2) === true);
console.log('3 is not even:', isEven(3) === false);

/*
5.421 Create a function called 'isOdd' similar to isEven. Can you use isEven to make this function?
*/
const isOdd = (x) => !isEven(x);
console.log('2 is not odd:', isOdd(2) === false);
console.log('3 is odd:', isOdd(3) === true);

/*
5.43 Create a function called 'isValidCreditCard' that takes a credit card number and returns true if it is valid, otherwise false. Refer to the credit card exercise in conditionals.
*/
const isValidCreditCard = creditCard => {
  return ((creditCard >= 10000 && creditCard < 100000)
          && (creditCard * 3) % 2 === 0
          && (creditCard % 5 === 0 || creditCard % 7 === 0));
};
console.log('123 is not valid:', isValidCreditCard(123) === false);
console.log('12345 is not valid:', isValidCreditCard(12345) === false);
console.log('86422 is valid:', isValidCreditCard(86422) === true);
console.log('86380 is valid:', isValidCreditCard(86380) === true);

/*
5.44 Create a function called 'dollarsToCents' that takes a value in dollars and returns the corresponding value in cents.
*/
const dollarsToCents = dollars => dollars * 100;
console.log('2 dollars = 200 cents:', dollarsToCents(2) === 200);

/*
5.45 Create a function called 'fToC' that takes a temperature in Fahrenheit and returns the temperature in Celsius.
*/
const fToC = ftemp => (ftemp - 32) / 1.8;
console.log('32 F = 0 C:', fToC(32) === 0);

/*
5.451 Create a function called 'cToF' that takes a temperature in Celsius and returns the temperature in Fahrenheit.
*/
const cToF = ctemp => (ctemp * 1.8) + 32;
console.log('0 C = 32 F:', cToF(0) === 32);

// Practice calling a function with multiple arguments

/*
5.5 Call the following function, providing a name and a language.
*/
const greetLanguage = (name, language) => {
  if (language === 'English') {
    console.log('Hello, ' + name + '!');
  } else if (language === 'French') {
    console.log('Bonjour, ' + name + '!');
  } else if (language === 'German') {
    console.log('Guten Tag, ' + name + '!');
  } else {
    console.log('Unknown language');
  }
};

greetLanguage('Alice', 'French');

/*
5.51 Write some lines of code to test if the following function multiplies 3 numbers together correctly.
*/
const multiplyAll = (a, b, c) => {
  return a * b * c;
};

console.log('1 * 2 * 3 = 6:', multiplyAll(1, 2, 3) === 6);

// Practice creating a function with multiple arguments

/*
5.6 Create a function called 'languageGreeting' that takes a name and a language and returns a greeting instead of printing it.

The following lines should help test if your function works correctly. They should print true.
*/
const languageGreeting = (name, language) => {
  if (language === 'English') {
    return 'Hello, ' + name + '!';
  } else if (language === 'French') {
    return 'Bonjour, ' + name + '!';
  } else if (language === 'German') {
    return 'Guten Tag, ' + name + '!';
  } else {
    return 'Unknown language';
  }
};

console.log('English greeting works:', languageGreeting('Alice', 'English') === 'Hello, Alice!');
console.log('French greeting works:', languageGreeting('Alice', 'French') === 'Bonjour, Alice!');
console.log('German greeting works:', languageGreeting('Alice', 'German') === 'Guten Tag, Alice!');
console.log('Unknown language works:', languageGreeting('Alice', 'gibberish') === 'Unknown language');

/*
5.61 Create a function called 'lovers' that takes two names and returns a string that the first name loves the second name.

The following lines should help test if your function works correctly. They should print true.
*/
const lovers = (name1, name2) => {
  return name1 + ' loves ' + name2;
};

console.log('Alice loves Bob:', lovers('Alice', 'Bob') === 'Alice loves Bob');
console.log('Bob loves Alice:', lovers('Bob', 'Alice') === 'Bob loves Alice');

/*
5.62 Create a function called 'convertTemperature' that takes a number and either 'FtoC' or 'CtoF', and returns the converted temperature. How can you use the functions from 5.45 and 5.451 to make this easier?

The following lines should help test if your function works correctly. They should print true.
*/
const convertTemperature = (temp, conv) => {
  if (conv === 'FtoC') {
    return fToC(temp);
  } else {
    return cToF(temp);
  }
};

console.log('32 F = 0 C:', convertTemperature(32, 'FtoC') === 0);
console.log('0 C = 32 F:', convertTemperature(0, 'CtoF') === 32);

/*
5.63 Create a function called 'divisibleBy' that takes two numbers and returns true if the first number is divisible by the second, otherwise false.

The following lines should help test if your function works correctly. They should print true.
*/
const divisibleBy = (x, y) => x % y === 0;

console.log('5 is not divisible by 3:', divisibleBy(5, 3) === false);
console.log('1 is divisible by 1:', divisibleBy(1, 1) === true);
console.log('6 is divisible by 2:', divisibleBy(6, 2) === true);
console.log('6 is divisible by 3:', divisibleBy(6, 3) === true);

/*
5.64 Create a function called 'bonusTime' that takes a number (salary) and a boolean (bonus). If the second argument is true, return the salary multiplied by 10, otherwise the original salary.

Write some lines of code to test if your function works correctly.
*/
const bonusTime = (salary, bonus) => {
  if (bonus) {
    return salary * 10;
  } else {
    return salary;
  }
};

console.log('10000 with bonus = 100000:', bonusTime(10000, true) === 100000);
console.log('10000 without bonus = 10000:', bonusTime(10000, false) === 10000);

/*
5.65 Create a function called 'rps' for playing the game Rock, Paper, Scissors. It should take two arguments, which should each be either 'rock', 'paper', or 'scissors'. If the first hand beats the second hand, return 1. If the first hand loses, return -1. In the case of a draw, return 0.

Write some lines of code to test if your function works correctly.
*/
const rps = (hand1, hand2) => {
  if (
    (hand1 === 'rock' && hand2 === 'scissors')
      || (hand1 === 'paper' && hand2 === 'rock')
      || (hand1 === 'scissors' && hand2 === 'paper')) {
    return 1;
  } else if (
    (hand2 === 'rock' && hand1 === 'scissors')
      || (hand2 === 'paper' && hand1 === 'rock')
      || (hand2 === 'scissors' && hand1 === 'paper')) {
    return -1;
  } else {
    return 0;
  }
};

console.log('rock beats scissors:', rps('rock', 'scissors') === 1);
console.log('paper loses to scissors:', rps('paper', 'scissors') === -1);
console.log('scissors and scissors draw:', rps('scissors', 'scissors') === 0);
