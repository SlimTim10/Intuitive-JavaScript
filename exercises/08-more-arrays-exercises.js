// Before anything else, we need a function that allows us to compare arrays so we can write better tests!

process.stdout.write('*8·01.\n');
/*
*8·01. Create a function called 'eqArrays' that takes two arrays and returns true if they are equal, otherwise false. Two arrays are considered equal if they are the same length and every element is equal (in the same order).

Tip: The every() method can use the array indexes as a second argument of the callback function. See the documentation for more detail:
https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/every

The following lines should help test if your function works correctly. They should print true.
*/


// console.log('eqArrays tests');
// console.log(eqArrays([], []));
// console.log(eqArrays([1, 2, 3], [1, 2, 3]));
// console.log(!eqArrays([1, 2, 3], [1, 3, 2]));
// console.log(!eqArrays([1, 2, 3], [1, 2, 3, 4]));
// console.log(!eqArrays([1, 2, 3, 4], [1, 2, 3]));
// console.log(eqArrays(['Alice', 'Bob', 'Carol'], ['Alice', 'Bob', 'Carol']));
// console.log(eqArrays([1, 2, 3, [4, 5, 6]], [1, 2, 3, [4, 5, 6]]));


// Practice using the some() method

process.stdout.write('\n*8·1.\n');
/*
*8·1. Create a function called 'anyOdd' that takes an array of numbers and returns true if any of them are odd, otherwise false.

The following lines should help test if your function works correctly. They should print true.
*/


// console.log('anyOdd tests');
// console.log(anyOdd([1, 2, 4, 6]));
// console.log(anyOdd([2, 4, 6, 7]));
// console.log(!anyOdd([2, 4, 6, 8]));


process.stdout.write('\n*8·11.\n');
/*
*8·11. Create a function called 'anyNegative' that takes an array of numbers and returns true if any of them are negative, otherwise false.

Remember to write tests!
*/


process.stdout.write('\n*8·12.\n');
/*
*8·12. Create a function called 'anyZs' that takes an array of words (strings) and returns true if the letter "z" (lowercase or uppercase) is found in any of the words, otherwise false.
*/


process.stdout.write('\n*8·13.\n');
/*
*8·13. Create a function called 'overTheLimit' that takes a limit (number) and an array of account balances (numbers). It should return true if any of the account balances is greater than the given limit, otherwise false.
*/


process.stdout.write('\n*8·14.\n');
/*
*8·14. Create a function called 'addNewLetterName' that takes a new name (string) and an array of names (strings). The goal is to add the new name to the names if there isn't already a name that starts with the same letter. The function should return a new array of names that contains the original names plus the new one if it starts with a unique letter, otherwise return the original names.

The following lines should help test if your function works correctly. They should print true.
*/


// console.log('addNewLetterName tests');
// console.log(eqArrays(
//   addNewLetterName('Bob', ['Alice', 'Carol', 'Dave']),
//   ['Alice', 'Carol', 'Dave', 'Bob']
// ));
// console.log(eqArrays(
//   addNewLetterName('Bob', ['Alice', 'Beatrice', 'Carol', 'Dave']),
//   ['Alice', 'Beatrice', 'Carol', 'Dave']
// ));


// Practice using the every() method

process.stdout.write('\n*8·2.\n');
/*
*8·2. Create a function called 'irishGroup' that takes an array of surnames (strings) and returns true if they all begin with "Mc", otherwise false.
*/


process.stdout.write('\n*8·21.\n');
/*
*8·21. Create a function called 'allWhole' that takes an array of numbers and returns true if they are all whole numbers, otherwise false. Whole numbers are the numbers starting from 0 and counting up forever: 0, 1, 2, 3, 4, 5, ... . Negative numbers and decimals (e.g. 1.5) are not whole numbers.
*/


process.stdout.write('\n*8·22.\n');
/*
*8·22. Create a function called 'britishGang' that takes an array of surnames (strings) and returns true if they are all likely British. A surname that is likely British starts with "Mac" or "Mc", or is any of the top 10 British surnames: Smith, Jones, Williams, Taylor, Davies, Brown, Wilson, Evans, Thomas, Johnson.

Tip: Consider making a helper function 'isBritish' to check if a single surname is British.
*/


// Practice using the map() method

process.stdout.write('\n*8·3.\n');
/*
*8·3. Create a function called 'convertMoney' to convert an array of values in dollars to an array of values in cents.

The following lines should help test if your function works correctly. They should print true.
*/


// console.log('convertMoney tests');
// console.log(eqArrays(convertMoney([2, 3.5, 10]), [200, 350, 1000]));
// console.log(eqArrays(convertMoney([0, 0.25, 99.99]), [0, 25, 9999]));


process.stdout.write('\n*8·31.\n');
/*
*8·31. Create a function called 'convertTemperatures' that takes an array of temperatures (numbers) and either 'FtoC' or 'CtoF', and returns an array of the converted temperatures.

The following lines should help test if your function works correctly. They should print true.
*/


// console.log('convertTemperatures tests');
// console.log(eqArrays(convertTemperatures([32, 41], 'FtoC'), [0, 5]));
// console.log(eqArrays(convertTemperatures([0, -10], 'CtoF'), [32, 14]));


process.stdout.write('\n*8·32.\n');
/*
*8·32. Create a function called 'bonusSalaries' that takes an array of salary-bonus pairs and returns the new salaries. Each salary-bonus pair is an array with a salary and a boolean that says whether or not the salary receives a bonus. A bonus salary is the salary multiplied by 10.

The following lines should help test if your function works correctly. They should print true.
 */


// console.log('bonusSalaries tests');
// console.log(eqArrays(
//   bonusSalaries([ [123, false], [123, true] ]),
//   [123, 1230]
// ));
// console.log(eqArrays(
//   bonusSalaries([ [10000, true], [10000, false], [30000, true], [100000, false], [64000.99, true] ]),
//   [100000, 10000, 300000, 100000, 640009.9]
// ));


process.stdout.write('\n*8·33.\n');
/*
*8·33. Create a function called 'rpsResults' that takes an array of "Rock, Paper, Scissors" games and returns an array of results. Each game is an array of two hands. Each hand is either 'rock', 'paper', or 'scissors'. If the first hand beats the second hand, the result is 1. If the first hand loses, the result is -1. In the case of a draw, the result is 0.

The following lines should help test if your function works correctly. They should print true.
*/


// console.log('rpsResults tests');
// console.log(eqArrays(
//   rpsResults([ ['rock', 'scissors'], ['paper', 'scissors'], ['scissors', 'scissors'] ]),
//   [1, -1, 0]
// ));
// console.log(eqArrays(
//   rpsResults([ ['rock', 'rock'], ['paper', 'paper'], ['scissors', 'scissors'], ['scissors', 'paper'], ['paper', 'rock'] ]),
//   [0, 0, 0, 1, 1]
// ));


process.stdout.write('\n*8·34.\n');
/*
 *8·34. Create a function called 'makeRectangles' that takes an array of numbers and returns an array of rectangles, where a rectangle is an array of two numbers: [length, width].

The following lines should help test if your function works correctly. They should print true.
*/


// console.log('makeRectangles tests');
// console.log(eqArrays(
//   makeRectangles([1, 2, 3, 4]),
//   [ [1, 1], [2, 2], [3, 3], [4, 4] ]
// ));
// console.log(eqArrays(
//   makeRectangles([-1, 0, 99, 1000]),
//   [ [-1, -1], [0, 0], [99, 99], [1000, 1000] ]
// ));


// Practice using the forEach() method

process.stdout.write('\n*8·4.\n');
/*
*8·4. Say hello to each of the names in the following array (e.g. Hello, Alice!).
*/
const names1 = ['Alice', 'Bob', 'Carol', 'Dave', 'Eve'];


process.stdout.write('\n*8·41.\n');
/*
*8·41. Greet each person in their own language (e.g. Bonjour, Alice!).
*/
const people1 = [['Alice', 'French'], ['Bob', 'English'], ['Carol', 'German']];


process.stdout.write('\n*8·42.\n');
/*
*8·42. Create a function called 'manyLovers' that takes an array of couples and prints that the first names love the second names (e.g. Alice loves Bob).
*/


// Practice using the filter() method

process.stdout.write('\n*8·5.\n');
/*
*8·5. Create a function called 'wholeNumbers' that takes an array of numbers and returns a new array containing only the numbers that are whole. Whole numbers are the numbers starting from 0 and counting up forever: 0, 1, 2, 3, 4, 5, ... . Negative numbers and decimals (e.g. 1.5) are not whole numbers.
*/


process.stdout.write('\n*8·51.\n');
/*
*8·51. Create a function called 'countNulls' that takes an array and returns how many null values are in it.
*/


process.stdout.write('\n*8·52.\n');
/*
*8·52. Create a function called 'mostlyScottish' that takes an array of surnames (strings) and returns true if more than half of them are Scottish, otherwise false. We'll say a Scottish surname is one that starts with "Mac".
*/


process.stdout.write('\n*8·53.\n');
/*
*8·53. Create a function called 'removeLetterNames' that takes a letter and an array of names (strings). It should return the names without any starting with the given letter.

The following lines should help test if your function works correctly. They should print true.
*/


// console.log('removeLetterNames');
// console.log(eqArrays(
//   removeLetterNames('B', ['Alice', 'Bob', 'Carol', 'Dave', 'Beatrice']),
//   ['Alice', 'Carol', 'Dave']
// ));
// console.log(eqArrays(
//   removeLetterNames('Z', ['Alice', 'Bob', 'Carol', 'Dave', 'Beatrice']),
//   ['Alice', 'Bob', 'Carol', 'Dave', 'Beatrice']
// ));


process.stdout.write('\n*8·54.\n');
/*
*8·54. Create a function called 'rpsPoints' that takes an array of rock-paper-scissors games and returns the number of games where the first player won.

The following lines should help test if your function works correctly. They should print true.
*/


// console.log('rpsPoints tests');
// console.log(rpsPoints([ ['paper', 'rock'] ]) === 1);
// console.log(rpsPoints([ ['paper', 'rock'], ['paper', 'paper'], ['scissors', 'rock'] ]) === 1);
// console.log(rpsPoints([ ['paper', 'rock'], ['rock', 'scissors'], ['scissors', 'paper'] ]) === 3);


// Practice using the reduce() method

process.stdout.write('\n*8·6.\n');
/*
*8·6. Create a function called 'sum' that takes an array of numbers and returns their sum, or 0 for an empty array.
*/


process.stdout.write('\n*8·601.\n');
/*
*8·601. Create a function called 'product' that takes an array of numbers and returns their product, or 1 for an empty array.
*/


process.stdout.write('\n*8·61.\n');
/*
*8·61. Create a function called 'duplicates' that takes an array and returns a new array containing each of the previous values twice.

The following lines should help test if your function works correctly. They should print true.
*/


// console.log('duplicates tests');
// console.log(eqArrays(
//   duplicates([1, 2, 3, 4]),
//   [1, 1, 2, 2, 3, 3, 4, 4]
// ));
// console.log(eqArrays(
//   duplicates(['Alice', 'Bob', 'Carol']),
//   ['Alice', 'Alice', 'Bob', 'Bob', 'Carol', 'Carol']
// ));


process.stdout.write('\n*8·62.\n');
/*
*8·62. Create a function called 'maximum' that takes an array of numbers and returns the highest number.
*/


process.stdout.write('\n*8·621.\n');
/*
*8·621. Create a function called 'minimum' that takes an array of numbers and returns the lowest number.
*/


process.stdout.write('\n*8·63.\n');
/*
*8·63. Create a function called 'dropRepeats' that takes an array and returns a new array without any repeating elements.

The following lines should help test if your function works correctly. They should print true.
*/


// console.log('dropRepeats tests');
// console.log(eqArrays(
//   dropRepeats([1, 1, 1, 2, 3, 4, 4, 2, 2]),
//   [1, 2, 3, 4]
// ));
// console.log(eqArrays(
//   dropRepeats([9, 8, 7, 8, 9]),
//   [9, 8, 7]
// ));


process.stdout.write('\n*8·64.\n');
/*
*8·64. Create a function called 'flatten' that takes a 2-dimensional array and returns a flattened (1-dimensional) array.

The following lines should help test if your function works correctly. They should print true.
*/


// console.log('flatten tests');
// console.log(eqArrays(
//   flatten([ [1, 2, 3], [4, 5, 6] ]),
//   [1, 2, 3, 4, 5, 6]
// ));
// console.log(eqArrays(
//   flatten([ ['Alice', 'Bob'], ['Carol', 'Dave'], ['Eve'] ]),
//   ['Alice', 'Bob', 'Carol', 'Dave', 'Eve']
// ));
// console.log(eqArrays(
//   flatten([[1, 2, 3], [4, 5, 6]]),
//   [1, 2, 3, 4, 5, 6]
// ));


process.stdout.write('\n*8·65.\n');
/*
*8·65. Create a function called 'totalMinutes' that takes an array of time pairs and returns the total minutes. Each time pair is two numbers: [hours, minutes].
*/


// Bonus exercises

process.stdout.write('\n*8·7.\n');
/*
*8·7. Create a function called 'intersperse' that takes a value and an array, and returns a new array with the value interspersed between each element of the original array.
*/


process.stdout.write('\n*8·71.\n');
/*
*8·71. Create a function called 'bigWordLetters' that takes an array of words (strings) and returns the total number of letters in all of the words that are more than 3 letters long.
*/


process.stdout.write('\n*8·72.\n');
/*
*8·72. Create a function called 'gamePoints' that takes an array of game results and returns the total points, according to the following description. Each game result is a pair of scores: [home team score, away team score]. Games where the home team won are worth 3 points. Games where the home team lost are worth 0 points. Tie games are worth 1 point.
*/


process.stdout.write('\n');
