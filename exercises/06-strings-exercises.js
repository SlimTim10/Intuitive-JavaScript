/*
Use MDN as a reference throughout these exercises.

https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String
*/

// Practice using string index

process.stdout.write('*6·1. ');
/*
*6·1. Print the third letter of the alphabet string.
*/
const alphabet = 'abcdefghijklmnopqrstuvwxyz';


process.stdout.write('\n*6·11. ');
/*
*6·11. Print the last letter of the alphabet string, without using the length property.
*/


process.stdout.write('\n*6·12.\n');
/*
*6·12. Create a function called 'numLetter' that takes a number, n, and returns the nth letter of the alphabet. If n is 3, it should return the third letter, 'c'.

Remember to write some lines of code to test that your function works correctly.
*/


process.stdout.write('\n*6·13.\n');
/*
*6·13. Create a function called 'isAtIndex' that takes a character, a number, and a string. It should return true if the character is found at the index number of the string, otherwise false.

The following lines should help test if your function works correctly. They should print true.
*/


// console.log('e is at index 1 in hello:', isAtIndex('e', 1, 'hello') === true);
// console.log('e is at index 4 in Alice:', isAtIndex('e', 4, 'Alice') === true);


// Practice using length

process.stdout.write('\n*6·2. ');
/*
*6·2. Print the length of the alphabet string.
*/


process.stdout.write('\n*6·21. ');
/*
*6·21. Print the last letter of the alphabet string, using the length property.
*/


process.stdout.write('\n*6·22. ');
/*
*6·22. Try to figure out the length of the string c in your head, then print it to see if you got it right.
*/
const a = 'alpha';
const b = 'bet';
const c = a + b;


// Practice using methods

process.stdout.write('\n*6·3.\n');
/*
*6·3. Create a function called 'inAlphabet' that takes a string and returns true if it is included in the alphabet, otherwise false. It only needs to work on lowercase letters.

Use the includes() method.

The following lines should help test if your function works correctly. They should print true.
*/


// console.log('a is in the alphabet:', inAlphabet('a') === true);
// console.log('lmno is in the alphabet:', inAlphabet('lmno') === true);
// console.log('1 is not in the alphabet:', inAlphabet('1') === false);


process.stdout.write('\n*6·301.\n');
/*
*6·301. Create a function called 'isDigit' that takes a digit as a string and returns true if it is included in the following string, otherwise false.

The following lines should help test if your function works correctly. They should print true.
*/
const digits = '1234567890';



// console.log('1 is a digit:', isDigit('1') === true);
// console.log('9 is a digit:', isDigit('9') === true);
// console.log('a is not a digit:', isDigit('a') === false);


process.stdout.write('\n*6·302.\n');
/*
*6·302. Create a function called 'isInTrouble' that takes a name and returns true if it is included in the email text.

Remember to write tests.
*/
const email = `Hello all.

As you know, last quarter was suboptimal. We did not meet the expected revenue and things need to change. There are a few people invited to a special meeting at 1PM today in the conference room. From engineering, Dilbert, Wally, and Alice. Asok the intern. Mordac from management.

If your name was not stated, continue work as usual. Sincerely, PHB.`;


process.stdout.write('\n*6·303.\n');
/*
*6·303. Create a function called 'badLanguage' that takes a message and returns true if it contains any bad language, otherwise false. The bad language phrases are: butt face, poopy head, and snot brain.
*/


process.stdout.write('\n*6·31.\n');
/*
*6·31. Create a function called 'IsBasicQuestion' that takes a sentence and returns true if it a basic question, otherwise false. A basic question begins with one of the five Ws (Who, What, When, Where, Why) and ends with a question mark.
*/


process.stdout.write('\n*6·32.\n');
/*
*6·32. Create a function called 'validURL' that takes a string and returns true if it is a valid URL, otherwise false. For our sake, a valid URL must start with either "http://" or "https://" and end with any of: ".com", ".ca", or ".org".
*/


process.stdout.write('\n*6·33.\n');
/*
*6·33. Create a function called 'firstSpace' that takes a string and returns the index of its first space character, or -1 if there is no space.
*/


process.stdout.write('\n*6·331.\n');
/*
*6·331. Create a function called 'firstOfLast' that takes a full name (e.g. "John Smith") and returns the first letter of the last name (e.g. "S"). How can you use the firstSpace() function to make this easier?
*/


process.stdout.write('\n*6·332.\n');
/*
*6·332. Create a function called 'initials' that takes a full name (e.g. "Jane Doe") and returns the initials (e.g. "J.D."). How can you use firstOfLast() to make this easier?
*/


process.stdout.write('\n*6·34.\n');
/*
*6·34. Create a function called 'capitalize' that takes a word (e.g. "hello") and returns the same word, but with the first letter capitalized (e.g. "Hello").
*/


process.stdout.write('\n*6·35.\n');
/*
*6·35. Create a function called 'rhetorical' that takes a rhetorical question and returns the same question, but ending with a period instead of a question mark.
*/


process.stdout.write('\n*6·36.\n');
/*
*6·36. Create a function called 'couldBeWord' that takes a made up word and returns true if it could be a word, otherwise false. Only made up words containing at least one vowel (a, e, i, o, u) could be words.
*/


process.stdout.write('\n*6·37.\n');
/*
*6·37. Create a function called 'fixSentence' that takes a sentence and returns the fixed version. The first letter should be capitalized, and a period should be added if the original does not end with punctuation (period, question mark, or exclamation mark).
*/


process.stdout.write('\n*6·38.\n');
/*
*6·38. Create a function called 'alternating' that takes a number and returns an alternating string of that many 1s and 0s.

1: 1
2: 10
3: 101
4: 1010
5: 10101
...
*/


process.stdout.write('\n*6·4.\n');
/*
*6·4. Create a function called 'hasDigit' that takes a string and returns true if it contains any digits, otherwise false.
*/


process.stdout.write('\n*6·41.\n');
/*
*6·41. Create a function called 'hasPunctuation' that takes a string and returns true if it contains any punctuation (., !, ?), otherwise false.
*/


process.stdout.write('\n*6·42.\n');
/*
*6·42. Create a function called 'validPassword' that takes a password and returns true if it is valid, otherwise false.

A valid password must:
- be at least 6 characters long
- contain a digit
- contain a character of punctuation (., !, ?)
*/


process.stdout.write('\n*6·43.\n');
/*
*6·43. Create a function called 'betterPassword' that takes two passwords and returns the better of the two. A password is considered better if it gets more points. Each character in the password counts for 1 point. Including any digits counts for an additional 5 points (flat, not for each digit). Including any punctuation (., !, ?) counts for an additional 10 points (flat, not for each punctuation).

For example, 'p4ssw0rd!' gets 9 + 5 + 10 = 24 points.
*/


process.stdout.write('\n');
