// We need a function that allows us to compare arrays so we can write better tests!

/*
8.01 Create a function called 'eqArrays' that takes two arrays and returns true if they are equal, otherwise false. Two arrays are considered equal if they are the same length and every element is equal (in the same order).

Tip: The every() method can use the array indexes as a second argument of the callback function. See the documentation for more detail:
https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/every

The following lines should help test if your function works correctly. They should print true.
*/


console.log('-- eqArrays tests');
console.log(eqArrays([], []));
console.log(eqArrays([1, 2, 3], [1, 2, 3]));
console.log(!eqArrays([1, 2, 3], [1, 3, 2]));
console.log(!eqArrays([1, 2, 3], [1, 2, 3, 4]));
console.log(!eqArrays([1, 2, 3, 4], [1, 2, 3]));
console.log(eqArrays(['Alice', 'Bob', 'Carol'], ['Alice', 'Bob', 'Carol']));
console.log(eqArrays([1, 2, 3, [4, 5, 6]], [1, 2, 3, [4, 5, 6]]));

// Practice using the some() method

/*
8.1 Create a function called 'anyOdd' that takes an array of numbers and returns true if any of them are odd, otherwise false.

The following lines should help test if your function works correctly. They should print true.
*/


console.log('-- anyOdd tests');
console.log(anyOdd([1, 2, 4, 6]));
console.log(anyOdd([2, 4, 6, 7]));
console.log(!anyOdd([2, 4, 6, 8]));

/*
8.11 Create a function called 'anyNegative' that takes an array of numbers and returns true if any of them are negative, otherwise false.
*/


/*
8.12 Create a function called 'anyZs' that takes an array of words (strings) and returns true if the letter "z" (lowercase or uppercase) is found in any of the words, otherwise false.
*/


/*
8.13 Create a function called 'overTheLimit' that takes a limit (number) and an array of account balances (numbers). It should return true if any of the account balances is greater than the given limit, otherwise false.
*/


// Practice using the every() method

/*
8.2 Create a function called 'irishGroup' that takes an array of surnames (strings) and returns true if they all begin with "Mc", otherwise false.
*/


/*
8.21 Create a function called 'allWhole' that takes an array of numbers and returns true if they are all whole numbers, otherwise false. Whole numbers are the numbers starting from 0 and counting up forever: 0, 1, 2, 3, 4, 5, ... . Negative numbers and decimals (e.g. 1.5) are not whole numbers.
*/


/*
8.22 Create a function called 'britishGang' that takes an array of surnames (strings) and returns true if they are all likely British. A surname that is likely British starts with "Mac" or "Mc", or is any of the top 10 British surnames: Smith, Jones, Williams, Taylor, Davies, Brown, Wilson, Evans, Thomas, Johson.

Tip: Consider making a helper function 'isBritish' to check if a single surname is British.
*/


// Practice using the map() method

/*
8.3 Create a function called 'convertMoney' to convert an array of values in dollars to an array of values in cents.

The following lines should help test if your function works correctly. They should print true.
*/


console.log('-- convertMoney tests');
console.log(eqArrays(convertMoney([2, 3.5, 10]), [200, 350, 1000]));
console.log(eqArrays(convertMoney([0, 0.25, 99.99]), [0, 25, 9999]));

/*
8.31 Create a function called 'convertTemperatures' that takes an array of temperatures (numbers) and either 'FtoC' or 'CtoF', and returns an array of the converted temperatures.

The following lines should help test if your function works correctly. They should print true.
*/


console.log('-- convertTemperatures tests');
console.log(eqArrays(convertTemperatures([32, 41], 'FtoC'), [0, 5]));
console.log(eqArrays(convertTemperatures([0, -10], 'CtoF'), [32, 14]));

/*
8.32 Create a function called 'bonusSalaries' that takes an array of salary-bonus pairs and returns the new salaries. Each salary-bonus pair is an array with a salary and a boolean that says whether or not the salary receives a bonus. A bonus salary is the salary multiplied by 10.

The following lines should help test if your function works correctly. They should print true.
 */


/*
8.33 Create a function called 'rpsResults' that takes an array of "Rock, Paper, Scissors" games and returns an array of results. Each game is an array of two hands. Each hand is either 'rock', 'paper', or 'scissors'. If the first hand beats the second hand, the result is 1. If the first hand loses, the result is -1. In the case of a draw, the result is 0.

The following lines should help test if your function works correctly. They should print true.
*/


console.log('-- rpsResults tests');
console.log(eqArrays(
  rpsResults([ ['rock', 'scissors'], ['paper', 'scissors'], ['scissors', 'scissors'] ]),
  [1, -1, 0]
));
console.log(eqArrays(
  rpsResults([ ['rock', 'rock'], ['paper', 'paper'], ['scissors', 'scissors'], ['scissors', 'paper'], ['paper', 'rock'] ]),
  [0, 0, 0, 1, 1]
));

/*
8.34 Create a function called 'makeSquares' that takes an array of numbers and returns an array of squares. A square is an array of two numbers: [length, width].

The following lines should help test if your function works correctly. They should print true.
*/


console.log('-- makeSquares tests');
console.log(eqArrays(
  makeSquares([1, 2, 3, 4]),
  [ [1, 1], [2, 2], [3, 3], [4, 4] ]
));
console.log(eqArrays(
  makeSquares([-1, 0, 99, 1000]),
  [ [-1, -1], [0, 0], [99, 99], [1000, 1000] ]
));

// Practice using the forEach() method

/*
8.4 Say hello to each of the names in the following array (e.g. Hello, Alice!).
*/
const names1 = ['Alice', 'Bob', 'Carol', 'Dave', 'Eve'];



/*
8.41 Greet each person in their own language (e.g. Bonjour, Alice!).
*/
const people1 = [['Alice', 'French'], ['Bob', 'English'], ['Carol', 'German']];



/*
8.42 Create a function called 'manyLovers' that takes an array of couples and prints that the first names love the second names (e.g. Alice loves Bob).
*/


manyLovers([['Alice', 'Bob'], ['Carol', 'Dave'], ['Eve', 'Frankie']]);
