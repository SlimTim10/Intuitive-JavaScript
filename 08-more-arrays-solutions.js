// Practice using the some() method

/*
8.1 Create a function called 'anyOdd' that takes an array of numbers and returns true if any of them are odd, otherwise false.

The following lines should help test if your function works correctly. They should print true.
*/
const isOdd = x => x % 2 !== 0;
const anyOdd = xs => xs.some(x => isOdd(x));

console.log('-- anyOdd tests');
console.log(anyOdd([1, 2, 4, 6]));
console.log(anyOdd([2, 4, 6, 7]));
console.log(!anyOdd([2, 4, 6, 8]));

/*
8.11 Create a function called 'anyNegative' that takes an array of numbers and returns true if any of them are negative, otherwise false.
*/
const anyNegative = xs => xs.some(x => x < 0);

console.log('-- anyNegative tests');
console.log(anyNegative([-1, 0, 1, 2, 3, 4, 5]));
console.log(anyNegative([0, 1, 2, 3, 4, -5]));
console.log(!anyNegative([0, 1, 2, 3, 4, 5]));

/*
8.12 Create a function called 'anyZs' that takes an array of words (strings) and returns true if the letter "z" is found in any of the words, otherwise false.
*/
const anyZs = words => words.some(word => word.includes('z'));

console.log('-- anyZs tests');
console.log(anyZs(['apple', 'banana', 'zucchini']));
console.log(anyZs(['apple', 'maze', 'carrot']));
console.log(!anyZs(['apple', 'banana', 'carrot']));

/*
8.13 Create a function called 'overTheLimit' that takes a limit (number) and an array of account balances (numbers). It should return true if any of the account balances is greater than the given limit, otherwise false.
*/
const overTheLimit = (limit, balances) => balances.some(balance => balance > limit);

console.log('-- overTheLimit tests');
console.log(overTheLimit(100, [2, 30, 99, 100, 101]));
console.log(!overTheLimit(100, [2, 30, 99, 100, -5]));
console.log(overTheLimit(100, [2, 3000, 99]));

// Practice using the every() method

/*
8.2 Create a function called 'irishGroup' that takes an array of surnames (strings) and returns true if they all begin with "Mc", otherwise false.
*/
const irishGroup = surnames => surnames.every(surname => surname.startsWith('Mc'));

console.log('-- irishGroup tests');
console.log(irishGroup(['McDonald', 'McQuarry', 'McInnis']));
console.log(!irishGroup(['McDonald', 'MacQuarry', 'McInnis']));
console.log(!irishGroup(['Jones', 'Freeman', 'Brown']));

/*
8.21 Create a function called 'allWhole' that takes an array of numbers and returns true if they are all whole numbers, otherwise false. Whole numbers are the numbers starting from 0 and counting up forever: 0, 1, 2, 3, 4, 5, ... . Negative numbers and decimals (e.g. 1.5) are not whole numbers.
*/
const allWhole = xs => xs.every(x => x >= 0 && x === Math.floor(x));

console.log('-- allWhole tests');
console.log(allWhole([1, 2, 3, 4, 5]));
console.log(!allWhole([1, 2.5, 3, 4, 5]));
console.log(!allWhole([1, 2, 3, 4, 5.01]));

/*
8.22 Create a function called 'britishGang' that takes an array of surnames (strings) and returns true if they are all likely British. A surname that is likely British starts with "Mac" or "Mc", or is any of the top 10 British surnames: Smith, Jones, Williams, Taylor, Davies, Brown, Wilson, Evans, Thomas, Johson.

Tip: Consider making a helper function 'isBritish' to check if a single surname is British.
*/
const britishSurnames = ['Smith', 'Jones', 'Williams', 'Taylor', 'Davies', 'Brown', 'Wilson', 'Evans', 'Thomas', 'Johnson'];
const isBritish = surname => surname.startsWith('Mc') || surname.startsWith('Mac') || britishSurnames.includes(surname);
const britishGang = surnames => surnames.every(isBritish);

console.log('-- britishGang tests');
console.log(britishGang(['MacDonald', 'McSorley', 'Taylor', 'Davies']));
console.log(!britishGang(['MacDonald', 'McSorley', 'Taylor', 'Davies', 'Freeman']));

/*
8.23 Create a function called 'eqArrays' that takes two arrays and returns true if they are equal, otherwise false. Two arrays are considered equal if they are the same length and every element is equal (in the same order).

Tip: The every() method can use the array indexes as a second argument of the callback function. See the documentation for more detail:
https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/every

The following lines should help test if your function works correctly. They should print true.
*/
const eqArrays = (xs, ys) => (xs.length === ys.length) && xs.every((_, i) => ys[i] === xs[i]);

console.log('-- eqArrays tests');
console.log(eqArrays([], []));
console.log(eqArrays([1, 2, 3], [1, 2, 3]));
console.log(!eqArrays([1, 2, 3], [1, 3, 2]));
console.log(!eqArrays([1, 2, 3], [1, 2, 3, 4]));
console.log(!eqArrays([1, 2, 3, 4], [1, 2, 3]));
console.log(eqArrays(['Alice', 'Bob', 'Carol'], ['Alice', 'Bob', 'Carol']));


// Practice using the filter() method

/*
8.? Create a function called 'mostlyScottish' that takes an array of surnames (strings) and returns true if more than half of them are Scottish. A Scottish surname is one that starts with "Mac".
*/
const mostlyScottish = surnames => surnames.filter(surname => surname.startsWith('Mac')).length > (surnames.length / 2);

console.log('-- mostlyScottish tests');
console.log(mostlyScottish(['MacDonald', 'MacQuarry', 'MacKenzie']));
console.log(mostlyScottish(['MacDonald', 'MacQuarry', 'MacKenzie', 'Jones']));
console.log(mostlyScottish(['MacDonald', 'MacQuarry', 'MacKenzie', 'Jones', 'Freeman']));
console.log(!mostlyScottish(['MacDonald', 'MacQuarry', 'MacKenzie', 'Jones', 'Freeman', 'Brown']));

/*
8.? Create a function called 'rpsPoints' that takes an array of rock-paper-scissors games and returns the number of games where the first player won.

The following lines should help test if your function works correctly. They should print true.
*/
const firstWon = ([player1, player2]) => (
  (player1 === 'rock' && player2 === 'scissors')
    || (player1 === 'paper' && player2 === 'rock')
    || (player1 === 'scissors' && player2 === 'paper')
);
const rpsPoints = games => games.filter(firstWon).length;

console.log('-- rpsPoints tests');
console.log(rpsPoints([ ['paper', 'rock'] ]) === 1);
console.log(rpsPoints([ ['paper', 'rock'], ['paper', 'paper'], ['scissors', 'rock'] ]) === 1);
console.log(rpsPoints([ ['paper', 'rock'], ['rock', 'scissors'], ['scissors', 'paper'] ]) === 3);
