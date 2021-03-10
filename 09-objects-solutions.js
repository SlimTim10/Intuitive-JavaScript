// Preliminary

/*
9.01 Before working with objects, we need a way to check if two objects are equal. There is a built-in function 'JSON.stringify' that takes an object and returns it as a string. We can use this to check if two objects are equal by simply comparing their string representations.

https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON/stringify

Create a function called 'eqObjects' that takes two objects and returns true if they are equal, otherwise false.

The following lines should help test if your function works correctly. They should print true.
*/
const eqObjects = (obj1, obj2) => JSON.stringify(obj1) === JSON.stringify(obj2);

console.log('-- eqObjects tests');
console.log(eqObjects(
  {a: 1, b: 2, c: 3},
  {a: 1, b: 2, c: 3}
));
console.log(!eqObjects(
  {a: 1, b: 2, c: 3},
  {a: 1, b: 4, c: 3}
));
console.log(eqObjects(
  {name: 'Alice', age: 45},
  {name: 'Alice', age: 45}
));
console.log(!eqObjects(
  {name: 'Alice', age: 45},
  {name: 'Bob', age: 45}
));
console.log(eqObjects(
  {name: 'Alice', age: 45, pets: ['dog', 'dog', 'cat']},
  {name: 'Alice', age: 45, pets: ['dog', 'dog', 'cat']}
));
console.log(!eqObjects(
  {name: 'Alice', age: 45, pets: ['dog', 'dog', 'cat']},
  {name: 'Alice', age: 45, pets: ['dog', 'dog']}
));

// Practice creating objects

/*
9.1 Create and print an object that has your name, favorite color, and favorite food.
*/
console.log({name: 'Tim Johns', favoriteColor: 'green', favoriteFood: 'ramen'});

/*
9.11 Create and print an array of objects of more people's information (as above).
*/
console.log([
  {name: 'Alice', favoriteColor: 'red', favoriteFood: 'sushi'},
  {name: 'Bob', favoriteColor: 'grey', favoriteFood: 'bread'},
]);

/*
9.12 Create and print an object using the following keys and values.
*/
const key1 = 'id';
const value1 = 5;
const key2 = 'name';
const value2 = 'Monitor stand';
const key3 = 'price';
const value3 = 57.99;

const product = {
  [key1]: value1,
  [key2]: value2,
  [key3]: value3,
};
console.log(product);

// Practice accessing values of properties in objects

/*
9.2 Print only the age of the following person.
*/
const alice = {name: 'Alice', age: 81, favoriteColor: 'red'};

console.log(alice.age);

/*
9.21 Print the value at the following key of the following email object.
*/
const key = 'subject';
const email = {
  from: 'alice@gmail.com',
  to: 'bob@gmail.com',
  subject: 'Quarterly Report',
  message: 'Please meet in the meeting room at 10am today to discuss the quarterly report.'
};

console.log(email[key]);

/*
9.22 Create a function called 'speciesOf' that takes a pet (object with name, species, and age) and returns its species.

The following lines should help test if your function works correctly. They should print true.
*/
const speciesOf = pet => pet.species;

console.log('-- speciesOf tests');
console.log(speciesOf({name: 'Nanimo', species: 'bearded dragon', age: 5}) === 'bearded dragon');
console.log(speciesOf({name: 'Marvin', species: 'cat', age: 8}) === 'cat');
console.log(speciesOf({name: 'Carl', species: 'dog', age: 2}) === 'dog');

/*
9.23 Create a function called 'greetPerson' that takes a person (object with first name and last name) and prints a friendly greeting. For example, "Hello, Dennis Ritchie!"
*/
const greetPerson = person => {
  console.log(`Hello, ${person.firstName} ${person.lastName}!`);
};
const greetPersonAlternate = ({firstName, lastName}) => {
  console.log(`Hello, ${firstName} ${lastName}!`);
};

console.log('-- greeting people');
greetPerson({firstName: 'Dennis', lastName: 'Ritchie'});
greetPerson({lastName: 'Thompson', firstName: 'Ken'});
greetPerson({age: 51, lastName: 'Torvalds', firstName: 'Linus'});

/*
9.24 Create a function called 'howManyCats' that takes a person (object with name, age, and cats) and returns the number of cats.

The following lines should help test if your function works correctly. They should print true.
*/
const howManyCats = person => person.cats.length;

console.log('-- howManyCats tests');
console.log(howManyCats({name: 'Alice', age: 36, cats: ['Cinnamon', 'Nutmeg']}) === 2);
console.log(howManyCats({name: 'Bob', age: 39, cats: ['Frank', 'Zeus', 'Markle', 'Percy']}) === 4);

/*
9.25 Create a function called 'enoughSun' that takes a current sunlight value (number from 0 to 10) and a plant (object with name and desired sunlight). It should return true if the amount of current sunlight is at least as high as the plant's desired sunlight.

The following lines should help test if your function works correctly. They should print true.
*/
const enoughSun = (currentSunlight, plant) => currentSunlight >= plant.sunlight;

console.log('-- enoughSun tests');
console.log(enoughSun(6, {name: 'blueberries', sunlight: 6}));
console.log(enoughSun(7, {name: 'moss', sunlight: 3}));
console.log(!enoughSun(2, {name: 'moss', sunlight: 3}));
console.log(!enoughSun(4, {name: 'blueberries', sunlight: 6}));

// Practice updating objects

/*
9.3 Create a function called 'resetScore' that takes a game (object with title and highScore) and returns the game with the high score set to 0.

The following lines should help test if your function works correctly. They should print true.
*/
const resetScore = game => {
  return {
    ...game,
    highScore: 0
  };
};

console.log('-- resetScore tests');
console.log(eqObjects(
  resetScore({title: 'Tetris', highScore: 999999}),
  {title: 'Tetris', highScore: 0}
));
console.log(eqObjects(
  resetScore({title: 'Super Mario Bros.', highScore: 1441150}),
  {title: 'Super Mario Bros.', highScore: 0}
));

/*
9.31 Create a function called 'fillDefaults' that takes a game (object with title and price) and returns the game with the following properties added to it: platform, set to 'Steam'; players, set to 1; rating, set to 'T'.
*/
const fillDefaults = game => {
  return {
    ...game,
    platform: 'Steam',
    players: 1,
    rating: 'T'
  };
};

console.log('-- fillDefaults tests');
console.log(eqObjects(
  fillDefaults({title: 'The Sims 4', price: 39.99}),
  {title: 'The Sims 4', price: 39.99, platform: 'Steam', players: 1, rating: 'T'}
));

/*
9.32 Create a function called 'setSalary' that takes a salary (number) and a person (object with name, title, and salary) and returns the person with the salary changed to the given value.

The following lines should help test if your function works correctly. They should print true.
*/
const setSalary = (salary, person) => {
  return {...person, salary};
};

console.log('-- setSalary tests');
console.log(eqObjects(
  setSalary(200000, {name: 'Alice', title: 'CEO', salary: 1000000}),
  {name: 'Alice', title: 'CEO', salary: 200000}
));
console.log(eqObjects(
  setSalary(90000, {name: 'Bob', title: 'Developer', salary: 70000}),
  {name: 'Bob', title: 'Developer', salary: 90000}
));

/*
9.33 Create a function called 'giveBonus' that takes a person (object with name, title, and salary) and returns the person with the salary multiplied by 10.
*/
const giveBonus = person => {
  return { ...person, salary: person.salary * 10 }
};

console.log('-- giveBonus tests');
console.log(eqObjects(
  giveBonus({name: 'Carol', title: 'Manager', salary: 80000}),
  {name: 'Carol', title: 'Manager', salary: 800000}
));
console.log(eqObjects(
  giveBonus({name: 'Dave', title: 'Intern', salary: 0}),
  {name: 'Dave', title: 'Intern', salary: 0}
));

/*
9.34 Create a function called 'halfOff' that takes a product (object with name and price) and returns the product with the price cut in half.
*/
const halfOff = product => {
  return {
    ...product,
    price: product.price / 2
  };
};

console.log('-- halfOff tests');
console.log(eqObjects(
  halfOff({name: 'Fancy Pen', price: 100}),
  {name: 'Fancy Pen', price: 50}
));
console.log(eqObjects(
  halfOff({name: 'Paper Clip', price: 0.50}),
  {name: 'Paper Clip', price: 0.25}
));

/*
9.35 Create a function called 'resetDate' that takes an object and sets its 'date' property to the current time. Use Date.now() to get the current time.

https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/now

The following lines should help test if your function works correctly. They should print true.
*/
const resetDate = obj => {
  return {
    ...obj,
    date: Date.now()
  };
};

console.log('-- resetDate tests');
const task1 = {date: Date.parse('08 Mar 2021 18:00:00 EST'), text: 'do laundry'};
const newTask1 = resetDate(task1);
console.log(newTask1.date > task1.date);
const task2 = {date: Date.parse('09 Mar 2071 14:00:00 EST'), text: 'write exercises'};
const newTask2 = resetDate(task2);
console.log(newTask2.date < task2.date);

/*
9.36 Create a function called 'removeProp' that takes a property name (string) and an object, and returns a copy of the object with the given property set to undefined.

The following lines should help test if your function works correctly. They should print true.
*/
const removeProp = (prop, obj) => {
  return {...obj, [prop]: undefined};
};

console.log('-- removeProp tests');
console.log(eqObjects(
  removeProp('password', {name: 'Tim', password: 'hunter2', title: 'Developer'}),
  {name: 'Tim', title:  'Developer', password: undefined}
));
console.log(eqObjects(
  removeProp('password', {name: 'Tim', password: 'hunter2', title: 'Developer'}),
  {name: 'Tim', title:  'Developer'}
));

// Practice using object methods: Object.keys(), Object.values(), Object.entries()

/*
9.4 Create a function called 'bigObject' that takes an object and returns true if it has 10 or more keys, otherwise false. Values don't matter.
*/
const bigObject = obj => Object.keys(obj).length >= 10;

console.log('-- bigObject tests');
console.log(bigObject({a: 1, b: 2, c: 3, d: 4, e: 5, f: 6, g: 7, h: 8, i: 9, j: 10}));
console.log(!bigObject({a: 1, b: 2, c: 3, d: 4, e: 5, f: 6, g: 7, h: 8, i: 9}));

/*
9.401 Create a function called 'validFarmAnimal' that takes an object and returns true if it is a valid farm animal, otherwise false. A farm animal object must have these keys to be valid: species, age, owner. Values don't matter.

The following lines should help test if your function works correctly. They should print true.
*/
const validFarmAnimal = animal => {
  const ks = Object.keys(animal);
  return ks.includes('species') && ks.includes('age') && ks.includes('owner');
};

console.log('-- validFarmAnimal tests');
console.log(validFarmAnimal({species: 'cow', age: 3, owner: 'Alice'}));
console.log(validFarmAnimal({age: 1, species: 'chicken', owner: 'Bob'}));
console.log(validFarmAnimal({age: 1, species: 'chicken', owner: 'Bob', name: 'Betty Boo'}));
console.log(!validFarmAnimal({species: 'chicken', owner: 'Bob', name: 'Betty Boo'}));
console.log(!validFarmAnimal({age: 1, owner: 'Bob', name: 'Betty Boo'}));
console.log(!validFarmAnimal({age: 1, species: 'chicken', name: 'Betty Boo'}));

/*
9.41 Create a function called 'anyDebt' that takes an account (object with multiple bank account balances) and returns true if any of the balances is negative.

The following lines should help test if your function works correctly. They should print true.
*/
const anyDebt = account => Object.values(account).some(balance => balance < 0);

console.log('-- anyDebt tests');
console.log(anyDebt({bank1: -100, bank2: 10000}));
console.log(anyDebt({checking: 10000, creditCard: -100}));
console.log(!anyDebt({checking: 10000, creditCard: 100}));

/*
9.411 Create a function called 'totalBalance' that takes an account (as before) and returns the sum of all balances.
*/
const totalBalance = account => Object.values(account).reduce((total, balance) => total + balance, 0);

console.log('-- totalBalance tests');
console.log(totalBalance({bank1: -100, bank2: 10000}) === 9900);
console.log(totalBalance({checking: 100, creditCard: 50}) === 150);

// Bonus exercises

/*
9.5 Create a function called 'makeAnimal' that takes a name (string) and returns a randomly generated farm animal.

The animal should have the properties:
- species: string of either 'horse', 'cow', 'chicken', or 'lamb'
- age: number between 0-10 (years)
- name: the given name
*/
const rand = (min, max) => Math.floor(Math.random() * (max + 1 - min) + min);
const possibleSpecies = ['horse', 'cow', 'chicken', 'lamb'];
const makeAnimal = name => {
  const species = possibleSpecies[rand(0, possibleSpecies.length - 1)];
  return {
    name,
    age: rand(0, 10),
    species
  };
};

console.log('-- makeAnimal tests');
const Bessie = makeAnimal('Bessie');
console.log(
  Bessie.name === 'Bessie'
    && (Bessie.age >= 0 && Bessie.age <= 10)
    && possibleSpecies.includes(Bessie.species)
);
const Henry = makeAnimal('Henry');
console.log(
  Henry.name === 'Henry'
    && (Henry.age >= 0 && Henry.age <= 10)
    && possibleSpecies.includes(Henry.species)
);

/*
9.51 Create a function called 'analyzeWardrobe' that takes a wardrobe object and prints some information to the user.

A wardrobe is an object with the properties:
- owner: object with name (string) and age (number)
- tops: array of colors (strings)
- pants: array of colors (strings)
- shorts: array of colors (strings)
- skirts: array of colors (strings)
- desiredNumberOfOutfits: number

The first thing it should print is a greeting to the owner of the wardrobe (e.g. "Hello, Alice!").

The wardrobe's contains a desired number of outfits. The actual number of outfits can be calculated by the equation: (number of tops * number of pants) + (number of tops * number of shorts) + (number of tops * number of skirts). If the actual number of outfits is at least as high as the desired number of outfits, it should print "Your desired number of outfits works!", otherwise "You need to add more clothing for your desired number of outfits to work."
*/
const analyzeWardrobe = wardrobe => {
  console.log(`Hello, ${wardrobe.owner.name}!`);

  const numOutfits = (
    wardrobe.tops.length * wardrobe.pants.length
      + wardrobe.tops.length * wardrobe.shorts.length
      + wardrobe.tops.length * wardrobe.skirts.length
  );

  if (numOutfits >= wardrobe.desiredNumberOfOutfits) {
    console.log('Your desired number of outfits works!');
  } else {
    console.log('You need to add more clothing for your desired number of outfits to work.')
  }
};

analyzeWardrobe({
  owner: {
    name: 'Alice',
    age: 22
  },
  tops: ['blue', 'white', 'cream'],
  pants: ['navy', 'blue'],
  shorts: ['navy'],
  skirts: ['navy', 'blue'],
  desiredNumberOfOutfits: 15
}); // works

analyzeWardrobe({
  owner: {
    name: 'Alice',
    age: 22
  },
  tops: ['blue', 'white', 'cream'],
  pants: ['navy', 'blue'],
  shorts: ['navy'],
  skirts: ['navy', 'blue'],
  desiredNumberOfOutfits: 16
}); // not enough clothing
