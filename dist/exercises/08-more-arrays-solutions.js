// We need a function that allows us to compare arrays so we can write better tests!

/*
8.01 Create a function called 'eqArrays' that takes two arrays and returns true if they are equal, otherwise false. Two arrays are considered equal if they are the same length and every element is equal (in the same order).

Tip: The every() method can use the array indexes as a second argument of the callback function. See the documentation for more detail:
https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/every

The following lines should help test if your function works correctly. They should print true.
*/
const eqArrays = (xs, ys) => (xs.length === ys.length) && xs.every((_, i) => {
  if (Array.isArray(xs[i]) && Array.isArray(ys[i])) {
    return eqArrays(xs[i], ys[i]);
  } else {
    return ys[i] === xs[i];
  }
});

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
8.12 Create a function called 'anyZs' that takes an array of words (strings) and returns true if the letter "z" (lowercase or uppercase) is found in any of the words, otherwise false.
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

/*
8.14 Create a function called 'addNewLetterName' that takes a new name (string) and an array of names (strings). The goal is to add the new name to the names if there isn't already a name that starts with the same letter. The function should return a new array of names that contains the original names plus the new one if it starts with a unique letter, otherwise return the original names.

The following lines should help test if your function works correctly. They should print true.
*/
const addNewLetterName = (newName, names) => {
  if (!names.some(name => name[0] === newName[0])) {
    return [...names, newName];
  } else {
    return names;
  }
};

console.log('-- addNewLetterName tests');
console.log(eqArrays(
  addNewLetterName('Bob', ['Alice', 'Carol', 'Dave']),
  ['Alice', 'Carol', 'Dave', 'Bob']
));
console.log(eqArrays(
  addNewLetterName('Bob', ['Alice', 'Beatrice', 'Carol', 'Dave']),
  ['Alice', 'Beatrice', 'Carol', 'Dave']
));

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

// Practice using the map() method

/*
8.3 Create a function called 'convertMoney' to convert an array of values in dollars to an array of values in cents.

The following lines should help test if your function works correctly. They should print true.
*/
const dollarsToCents = dollars => dollars * 100;
const convertMoney = dollarValues => dollarValues.map(dollarsToCents);

console.log('-- convertMoney tests');
console.log(eqArrays(convertMoney([2, 3.5, 10]), [200, 350, 1000]));
console.log(eqArrays(convertMoney([0, 0.25, 99.99]), [0, 25, 9999]));

/*
8.31 Create a function called 'convertTemperatures' that takes an array of temperatures (numbers) and either 'FtoC' or 'CtoF', and returns an array of the converted temperatures.

The following lines should help test if your function works correctly. They should print true.
*/
const fToC = ftemp => (ftemp - 32) / 1.8;
const cToF = ctemp => (ctemp * 1.8) + 32;
const convertTemperature = (temp, conv) => {
  if (conv === 'FtoC') {
    return fToC(temp);
  } else {
    return cToF(temp);
  }
};
const convertTemperatures = (temps, conv) => temps.map(temp => convertTemperature(temp, conv));

console.log('-- convertTemperatures tests');
console.log(eqArrays(convertTemperatures([32, 41], 'FtoC'), [0, 5]));
console.log(eqArrays(convertTemperatures([0, -10], 'CtoF'), [32, 14]));

/*
8.32 Create a function called 'bonusSalaries' that takes an array of salary-bonus pairs and returns the new salaries. Each salary-bonus pair is an array with a salary and a boolean that says whether or not the salary receives a bonus. A bonus salary is the salary multiplied by 10.

The following lines should help test if your function works correctly. They should print true.
 */
const bonusTime = (salary, bonus) => {
  if (bonus) {
    return salary * 10;
  } else {
    return salary;
  }
};
const bonusSalaries = salaries => salaries.map(([salary, bonus]) => bonusTime(salary, bonus));

console.log('-- bonusSalaries tests');
console.log(eqArrays(
  bonusSalaries([ [123, false], [123, true] ]),
  [123, 1230]
));
console.log(eqArrays(
  bonusSalaries([ [10000, true], [10000, false], [30000, true], [100000, false], [64000.99, true] ]),
  [100000, 10000, 300000, 100000, 640009.9]
));

/*
8.33 Create a function called 'rpsResults' that takes an array of "Rock, Paper, Scissors" games and returns an array of results. Each game is an array of two hands. Each hand is either 'rock', 'paper', or 'scissors'. If the first hand beats the second hand, the result is 1. If the first hand loses, the result is -1. In the case of a draw, the result is 0.

The following lines should help test if your function works correctly. They should print true.
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
const rpsResults = games => games.map(([hand1, hand2]) => rps(hand1, hand2));

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
const makeSquares = xs => xs.map(x => [x, x]);

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

names1.forEach(name => console.log(`Hello, ${name}!`));

/*
8.41 Greet each person in their own language (e.g. Bonjour, Alice!).
*/
const people1 = [['Alice', 'French'], ['Bob', 'English'], ['Carol', 'German']];

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
people1.forEach(([name, language]) => greetLanguage(name, language));

/*
8.42 Create a function called 'manyLovers' that takes an array of couples and prints that the first names love the second names (e.g. Alice loves Bob).
*/
const lovers = (name1, name2) => {
  return name1 + ' loves ' + name2;
};
const manyLovers = couples => couples.forEach(([name1, name2]) => console.log(lovers(name1, name2)));

manyLovers([['Alice', 'Bob'], ['Carol', 'Dave'], ['Eve', 'Frankie']]);

// Practice using the filter() method

/*
8.5 Create a function called 'wholeNumbers' that takes an array of numbers and returns a new array containing only the numbers that are whole. Whole numbers are the numbers starting from 0 and counting up forever: 0, 1, 2, 3, 4, 5, ... . Negative numbers and decimals (e.g. 1.5) are not whole numbers.
*/
const wholeNumbers = xs => xs.filter(x => x >= 0 && x === Math.floor(x));

console.log('-- wholeNumbers tests');
console.log(eqArrays(
  wholeNumbers([-1, 0, 1, 2, 3, 4.5, 5, 6.01, 999]),
  [0, 1, 2, 3, 5, 999]
));
console.log(eqArrays(
  wholeNumbers([-1, 0.9, 1.2, 99.99]),
  []
));

/*
8.51 Create a function called 'countNulls' that takes an array and returns how many null values are in it.
*/
const countNulls = xs => xs.filter(x => x === null).length;

console.log('-- countNulls tests');
console.log(countNulls([1, 2, 3, null, 4, 5, null, null, null]) === 4);
console.log(countNulls([1, 2, 3, 4, 5]) === 0);

/*
8.52 Create a function called 'mostlyScottish' that takes an array of surnames (strings) and returns true if more than half of them are Scottish, otherwise false. A Scottish surname is one that starts with "Mac".
*/
const mostlyScottish = surnames => {
  const scots = surnames.filter(surname => surname.startsWith('Mac'));
  const half = surnames.length / 2;
  return scots.length > half;
}

console.log('-- mostlyScottish tests');
console.log(mostlyScottish(['MacDonald', 'MacQuarry', 'MacKenzie']));
console.log(mostlyScottish(['MacDonald', 'MacQuarry', 'MacKenzie', 'Jones']));
console.log(mostlyScottish(['MacDonald', 'MacQuarry', 'MacKenzie', 'Jones', 'Freeman']));
console.log(!mostlyScottish(['MacDonald', 'MacQuarry', 'MacKenzie', 'Jones', 'Freeman', 'Brown']));

/*
8.53 Create a function called 'removeLetterNames' that takes a letter and an array of names (strings). It should return the names without any starting with the given letter.

The following lines should help test if your function works correctly. They should print true.
*/
const removeLetterNames = (letter, names) => names.filter(name => name[0] !== letter);

console.log('-- removeLetterNames');
console.log(eqArrays(
  removeLetterNames('B', ['Alice', 'Bob', 'Carol', 'Dave', 'Beatrice']),
  ['Alice', 'Carol', 'Dave']
));
console.log(eqArrays(
  removeLetterNames('Z', ['Alice', 'Bob', 'Carol', 'Dave', 'Beatrice']),
  ['Alice', 'Bob', 'Carol', 'Dave', 'Beatrice']
));

/*
8.54 Create a function called 'rpsPoints' that takes an array of rock-paper-scissors games and returns the number of games where the first player won.

The following lines should help test if your function works correctly. They should print true.
*/
const rpsPoints = games => games.filter(([player1, player2]) => rps(player1, player2) === 1).length;

console.log('-- rpsPoints tests');
console.log(rpsPoints([ ['paper', 'rock'] ]) === 1);
console.log(rpsPoints([ ['paper', 'rock'], ['paper', 'paper'], ['scissors', 'rock'] ]) === 1);
console.log(rpsPoints([ ['paper', 'rock'], ['rock', 'scissors'], ['scissors', 'paper'] ]) === 3);

// Practice using the reduce() method

/*
8.6 Create a function called 'sum' that takes an array of numbers and returns their sum, or 0 for an empty array.
*/
const sum = xs => xs.reduce((acc, x) => acc + x, 0);

console.log('-- sum tests');
console.log(sum([1, 2, 3, 4, 5]) === 15);
console.log(sum([]) === 0);
console.log(sum([-1, -3]) === -4);

/*
8.601 Create a function called 'product' that takes an array of numbers and returns their product, or 1 for an empty array.
*/
const product = xs => xs.reduce((acc, x) => acc * x, 1);

console.log('-- product tests');
console.log(product([1, 2, 3, 4, 5]) === 120);
console.log(product([]) === 1);
console.log(product([-1, -3]) === 3);

/*
8.61 Create a function called 'duplicates' that takes an array and returns a new array containing each of the previous values twice.

The following lines should help test if your function works correctly. They should print true.
*/
const duplicates = xs => xs.reduce((acc, x) => [...acc, x, x], []);

console.log('-- duplicates tests');
console.log(eqArrays(
  duplicates([1, 2, 3, 4]),
  [1, 1, 2, 2, 3, 3, 4, 4]
));
console.log(eqArrays(
  duplicates(['Alice', 'Bob', 'Carol']),
  ['Alice', 'Alice', 'Bob', 'Bob', 'Carol', 'Carol']
));

/*
8.62 Create a function called 'maximum' that takes an array of numbers and returns the highest number.
*/
const maximum = xs => xs.reduce((acc, x) => Math.max(acc, x), xs[0]);

console.log('-- maximum tests');
console.log(maximum([1, 2, 3, 4, 5]) === 5);
console.log(maximum([3, 4, 2, 5, 1]) === 5);
console.log(maximum([-9, 100, 0]) === 100);

/*
8.621 Create a function called 'minimum' that takes an array of numbers and returns the lowest number.
*/
const minimum = xs => xs.reduce((acc, x) => Math.min(acc, x), xs[0]);

console.log('-- minimum tests');
console.log(minimum([1, 2, 3, 4, 5]) === 1);
console.log(minimum([3, 4, 2, 5, 1]) === 1);
console.log(minimum([-9, 100, 0]) === -9);

/*
8.63 Create a function called 'dropRepeats' that takes an array and returns a new array without any repeating elements.

The following lines should help test if your function works correctly. They should print true.
*/
const dropRepeats = xs => xs.reduce((acc, x) => {
  if (!acc.includes(x)) {
    return [...acc, x];
  } else {
    return acc;
  }
}, []);

console.log('-- dropRepeats tests');
console.log(eqArrays(
  dropRepeats([1, 1, 1, 2, 3, 4, 4, 2, 2]),
  [1, 2, 3, 4]
));
console.log(eqArrays(
  dropRepeats([9, 8, 7, 8, 9]),
  [9, 8, 7]
));

/*
8.64 Create a function called 'flatten' that takes a 2-dimensional array and returns a flattened (1-dimensional) array.

The following lines should help test if your function works correctly. They should print true.
*/
const flatten = xss => xss.reduce((acc, xs) => [...acc, ...xs], []);

console.log('-- flatten tests');
console.log(eqArrays(
  flatten([ [1, 2, 3], [4, 5, 6] ]),
  [1, 2, 3, 4, 5, 6]
));
console.log(eqArrays(
  flatten([ ['Alice', 'Bob'], ['Carol', 'Dave'], ['Eve'] ]),
  ['Alice', 'Bob', 'Carol', 'Dave', 'Eve']
));
console.log(eqArrays(
  flatten([[1, 2, 3], [4, 5, 6]]),
  [1, 2, 3, 4, 5, 6]
));

/*
8.65 Create a function called 'totalMinutes' that takes an array of time pairs and returns the total minutes. Each time pair is two numbers: [hours, minutes].
*/
const totalMinutes = timePairs => timePairs.reduce((total, [hours, minutes]) => total + hours*60 + minutes, 0);

console.log('-- totalMinutes tests');
console.log(totalMinutes([ [1, 0], [0, 30] ]) === 90);
console.log(totalMinutes([ [0, 5], [0, 10], [2, 10] ]) === 145);
console.log(totalMinutes([ [2, 33], [3, 44] ]) === 377);

// Bonus exercises

/*
8.7 Create a function called 'intersperse' that takes a value and an array, and returns a new array with the value interspersed between each element of the original array.
*/
const head = xs => xs[0];
const tail = xs => xs.slice(1);
const intersperse = (y, xs) => [head(xs), ...tail(xs).reduce((acc, x) => [...acc, y, x], [])];

console.log('-- intersperse tests');
console.log(eqArrays(
  intersperse(0, [1, 2, 3]),
  [1, 0, 2, 0, 3]
));
console.log(eqArrays(
  intersperse('a', ['b', 'n', 'n', 's']),
  ['b', 'a', 'n', 'a', 'n', 'a', 's']
));

/*
8.71 Create a function called 'bigWordLetters' that takes an array of words (strings) and returns the total number of letters in all of the words that are more than 3 letters long.
*/
const bigWordLetters = words => {
  const bigWords = words.filter(word => word.length > 3);
  const bigWordLengths = bigWords.map(word => word.length);
  return sum(bigWordLengths);
};

console.log('-- bigWordLetters tests');
console.log(bigWordLetters(['only', 'the', 'big', 'words', 'should', 'be', 'counted']) === 22);
console.log(bigWordLetters(['the', 'big', 'be']) === 0);

/*
8.72 Create a function called 'gamePoints' that takes an array of game results and returns the total points, according to the following description. Each game result is a pair of scores: [home team score, away team score]. Games where the home team won are worth 3 points. Games where the home team lost are worth 0 points. Tie games are worth 1 point.
*/
const gamePoints = ([homeScore, awayScore]) => {
  if (homeScore > awayScore) {
    return 3;
  } else if (homeScore < awayScore) {
    return 0;
  } else {
    return 1;
  }
};
const points = games => games.reduce((total, game) => total + gamePoints(game), 0);

console.log('-- points tests');
console.log(points([ [1, 0], [2, 0], [3, 0] ]) === 9);
console.log(points([ [1, 1], [2, 2], [3, 3] ]) === 3);
console.log(points([ [0, 1], [0, 2], [0, 3] ]) === 0);
console.log(points([ [1, 0], [4, 2], [3, 2], [2, 3], [2, 2], [0, 2] ]) === 10);
