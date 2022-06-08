/*
Use MDN as a reference throughout these exercises.

https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String
*/

// Practice using string index

/*
6.1 Print the third letter of the alphabet string.
*/
const alphabet = 'abcdefghijklmnopqrstuvwxyz';

console.log('Third letter of alphabet:', alphabet[2]);

/*
6.11 Print the last letter of the alphabet string, without using the length property.
*/
console.log('Last letter of alphabet:', alphabet[25]);

/*
6.12 Create a function called 'numLetter' that takes a number, n, and returns the nth letter of the alphabet. If n is 3, it should return the third letter, 'c'.

Remember to write some lines of code to test if your function works correctly.
*/
const numLetter = n => alphabet[n - 1];

console.log('third letter = c:', numLetter(3) === 'c');

/*
6.13 Create a function called 'isAtIndex' that takes a character, a number, and a string. It should return true if the character is found at the index number of the string, otherwise false.

The following lines should help test if your function works correctly. They should print true.
*/
const isAtIndex = (letter, index, str) => str[index] === letter;

console.log('e is at index 1 in hello:', isAtIndex('e', 1, 'hello') === true);
console.log('e is at index 4 in Alice:', isAtIndex('e', 4, 'Alice') === true);

// Practice using length

/*
6.2 Print the length of the alphabet string.
*/
console.log('Length of alphabet:', alphabet.length);

/*
6.21 Print the last letter of the alphabet string, using the length property.
*/
console.log('Last letter of alphabet:', alphabet[alphabet.length - 1]);

/*
6.22 Try to figure out the length of the string c in your head, then print it to see if you got it right.
*/
const a = 'alpha';
const b = 'bet';
const c = a + b;

console.log('a + b length:', c.length);

// Practice using methods

/*
6.3 Create a function called 'inAlphabet' that takes a string and returns true if it is included in the alphabet, otherwise false. It only needs to work on lowercase letters.

Use the includes() method.

The following lines should help test if your function works correctly. They should print true.
*/
const inAlphabet = letter => alphabet.includes(letter);

console.log('a is in the alphabet:', inAlphabet('a') === true);
console.log('lmno is in the alphabet:', inAlphabet('lmno') === true);
console.log('1 is not in the alphabet:', inAlphabet('1') === false);

/*
6.301 Create a function called 'isDigit' that takes a digit as a string and returns true if it is included in the following string, otherwise false.

The following lines should help test if your function works correctly. They should print true.
*/
const digits = '1234567890';

const isDigit = digit => digits.includes(digit);

console.log('1 is a digit:', isDigit('1') === true);
console.log('9 is a digit:', isDigit('9') === true);
console.log('a is not a digit:', isDigit('a') === false);

/*
6.302 Create a function called 'isInTrouble' that takes a name and returns true if it is included in the email text.
*/
const email = `Hello all.

As you know, last quarter was suboptimal. We did not meet the expected revenue and things need to change. There are a few people invited to a special meeting at 1PM today in the conference room. From engineering, Dilbert, Wally, and Alice. Asok the intern. Mordac from management.

If your name was not stated, continue work as usual. Sincerely, PHB.`;

const isInTrouble = name => email.includes(name);

console.log('Dilbert is in trouble:', isInTrouble('Dilbert') === true);
console.log('Alice is in trouble:', isInTrouble('Alice') === true);
console.log('Asok is in trouble:', isInTrouble('Asok') === true);
console.log('Sally is in trouble:', isInTrouble('Sally') === false);

/*
6.303 Create a function called 'badLanguage' that takes a message and returns true if it contains any bad language, otherwise false. The bad language phrases are: butt face, poopy head, and snot brain.
*/
const badLanguage = message => {
  return message.includes('butt face')
    || message.includes('poopy head')
    || message.includes('snot brain');
};

console.log('badLanguage tests');
console.log(badLanguage('Steven is a poopy head!') === true);
console.log(badLanguage('My pants are poopy from the scary movie') === false);
console.log(badLanguage('Sally was being a butt face to me today') === true);
console.log(badLanguage('Everyone knows Tim is a snot brain.') === true);

/*
6.31 Create a function called 'IsBasicQuestion' that takes a sentence and returns true if it a basic question, otherwise false. A basic question begins with one of the five Ws (Who, What, When, Where, Why) and ends with a question mark.
*/
const isBasicQuestion = sentence => {
  return (
    sentence.startsWith('Who')
      || sentence.startsWith('What')
      || sentence.startsWith('When')
      || sentence.startsWith('Where')
      || sentence.startsWith('Why')
  ) && sentence.endsWith('?');
};

console.log('isBasicQuestion tests');
console.log(isBasicQuestion('Who was at the scene?') === true);
console.log(isBasicQuestion('What happened at the scene?') === true);
console.log(isBasicQuestion('When did it take place?') === true);
console.log(isBasicQuestion('Where did it take place?') === true);
console.log(isBasicQuestion('Why did anything happen?') === true);
console.log(isBasicQuestion('Are you sure it happened at all?') === false);
console.log(isBasicQuestion('Who done it.') === false);


/*
6.32 Create a function called 'validURL' that takes a string and returns true if it is a valid URL, otherwise false. For our sake, a valid URL must start with either "http://" or "https://" and end with any of: ".com", ".ca", or ".org".
*/
const validURL = url => {
  return (
    url.startsWith('http://') || url.startsWith('https://')
  ) && (
    url.endsWith('.com') || url.endsWith('.ca') || url.endsWith('.org')
  );
};

console.log('validURL tests');
console.log(validURL('http://www.google.com') === true);
console.log(validURL('https://www.google.com') === true);
console.log(validURL('http://www.google') === false);
console.log(validURL('www.google.com') === false);
console.log(validURL('google') === false);
console.log(validURL('httpss://www.google.com') === false);

/*
6.33 Create a function called 'firstSpace' that takes a string and returns the index of its first space character, or -1 if there is no space.
*/
const firstSpace = str => str.indexOf(' ');

console.log('firstSpace tests');
console.log(firstSpace('this is a test') === 4);
console.log(firstSpace(' this is a test') === 0);

/*
6.331 Create a function called 'firstOfLast' that takes a full name (e.g. "John Smith") and returns the first letter of the last name (e.g. "S"). How can you use the firstSpace() function to make this easier?
*/
const firstOfLast = name => name[firstSpace(name) + 1];

console.log('firstOfLast tests');
console.log(firstOfLast('John Smith') === 'S');

/*
6.332 Create a function called 'initials' that takes a full name (e.g. "Jane Doe") and returns the initials (e.g. "J.D."). How can you use firstOfLast() to make this easier?
*/
const initials = name => `${name[0]}.${firstOfLast(name)}.`;

console.log('initials tests');
console.log(initials('Jane Doe') === 'J.D.');

/*
6.34 Create a function called 'capitalize' that takes a word (e.g. "hello") and returns the same word, but with the first letter capitalized (e.g. "Hello").
*/
const capitalize = word => word[0].toUpperCase() + word.slice(1);

console.log('capitalize tests');
console.log(capitalize('hello') === 'Hello');

/*
-- NEED TO CHANGE -- due to other solution: question.replace('?', '.')
6.35 Create a function called 'rhetorical' that takes a rhetorical question and returns the same question, but ending with a period instead of a question mark.
*/
const rhetorical = question => question.slice(0, question.length - 1) + '.';

console.log('rhetorical tests');
console.log(rhetorical('Do pigs fly?') === 'Do pigs fly.');
console.log(rhetorical('Are you serious?') === 'Are you serious.');

/*
6.36 Create a function called 'couldBeWord' that takes a made up word and returns true if it could be a word, otherwise false. Only made up words containing at least one vowel (a, e, i, o, u) could be words.
*/
const couldBeWord = word => word.includes('a') || word.includes('e') || word.includes('i') || word.includes('o') || word.includes('u');

console.log('zxcv could not be a word:', couldBeWord('zxcv') === false);
console.log('bax could be a word:', couldBeWord('bax') === true);
console.log('irph could be a word:', couldBeWord('irph') === true);
console.log('bnug could be a word:', couldBeWord('bnug') === true);
console.log('jimjak could be a word:', couldBeWord('jimjak') === true);

/*
6.37 Create a function called 'fixSentence' that takes a sentence and returns the fixed version. The first letter should be capitalized, and a period should be added if the original does not end with punctuation (period, question mark, or exclamation mark).
*/
const fixSentence = sentence => {
  const firstPart = sentence[0].toUpperCase() + sentence.slice(1);
  if (sentence.endsWith('.') || sentence.endsWith('?') || sentence.endsWith('!')) {
    return firstPart;
  } else {
    return firstPart + '.';
  }
};

console.log('fixSentence tests');
console.log(fixSentence('hey, nice chatting with you again!') == 'Hey, nice chatting with you again!');
console.log(fixSentence('hey, nice chatting with you again') == 'Hey, nice chatting with you again.');
console.log(fixSentence('how have you been?') == 'How have you been?');

/*
6.38 Create a function called 'alternating' that takes a number and returns an alternating string of that many 1s and 0s.

1: 1
2: 10
3: 101
4: 1010
5: 10101
...
*/
const alternating = n => {
  const evens = '10'.repeat(n / 2);
  if (n % 2 === 0) {
    return evens;
  } else {
    return evens + '1';
  }
};

console.log('alternating tests');
console.log(alternating(1) === '1');
console.log(alternating(4) === '1010');
console.log(alternating(5) === '10101');
console.log(alternating(10) === '1010101010');

/*
6.4 Create a function called 'hasDigit' that takes a string and returns true if it contains any digits, otherwise false.
*/
const hasDigit = str => {
  return (
    str.includes('1')
      || str.includes('2')
      || str.includes('3')
      || str.includes('4')
      || str.includes('5')
      || str.includes('6')
      || str.includes('7')
      || str.includes('8')
      || str.includes('9')
      || str.includes('0')
  );
};

console.log('1234 has a digit:', hasDigit('1234') === true);
console.log('abcd1 has a digit:', hasDigit('abcd1') === true);
console.log('1abcd has a digit:', hasDigit('1abcd') === true);
console.log('abcd has no digits:', hasDigit('abcd') === false);

/*
6.41 Create a function called 'hasPunctuation' that takes a string and returns true if it contains any punctuation (., !, ?), otherwise false.
*/
const hasPunctuation = str => str.includes('.') || str.includes('!') || str.includes('?');

console.log('hasPunctuation tests');
console.log(hasPunctuation('abcd.') === true);
console.log(hasPunctuation('.abcd') === true);
console.log(hasPunctuation('a.bcd') === true);
console.log(hasPunctuation('abcd?') === true);
console.log(hasPunctuation('a.bcd?') === true);
console.log(hasPunctuation('abcd') === false);

/*
6.42 Create a function called 'validPassword' that takes a password and returns true if it is valid, otherwise false.

A valid password must:
- be at least 6 characters long
- contain a digit
- contain a character of punctuation (., !, ?)
*/
const validPassword = pass => pass.length >= 6 && hasDigit(pass) && hasPunctuation(pass);

console.log('validPassword tests');
console.log(validPassword('abcd1.') === true);
console.log(validPassword('bcd1.') === false);
console.log(validPassword('!a1b.c?d2') === true);

/*
6.43 Create a function called 'betterPassword' that takes two passwords and returns the better of the two. A password is considered better if it gets more points. Each character in the password counts for 1 point. Including any digits counts for an additional 5 points (flat, not for each digit). Including any punctuation (., !, ?) counts for an additional 10 points (flat, not for each punctuation).

For example, 'p4ssw0rd!' gets 9 + 5 + 10 = 24 points.
*/

// Helper function to calculate the points of a password
const calcPoints = password => {
  const lengthPoints = password.length;
  const digitPoints = hasDigit(password) ? 5 : 0;
  const puncPoints = hasPunctuation(password) ? 10 : 0;
  return lengthPoints + digitPoints + puncPoints;
};

const betterPassword = (pass1, pass2) => {
  if (calcPoints(pass1) > calcPoints(pass2)) {
    return pass1;
  } else {
    return pass2;
  }
};

console.log('abcd is a better password than abc:', betterPassword('abcd', 'abc') === 'abcd');
console.log('abc123 is a better password than a!b:', betterPassword('abc123def456ghi', 'a!b') === 'abc123def456ghi');
console.log('a!bcd is a better password than abc123def:', betterPassword('a!bcd', 'abc123def') === 'a!bcd');
