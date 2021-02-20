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
8.12 Create a function called 'anyZs' that takes an array of words (strings) and returns true if the letter "z" is found in any of the words, otherwise false.
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


/*
8.23 Create a function called 'eqArrays' that takes two arrays and returns true if they are equal, otherwise false. Two arrays are considered equal if they are the same length and every element is equal (in the same order).

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
