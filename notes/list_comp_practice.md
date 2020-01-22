# List Comprehension Practice

The library database example:

- We represent persons and books simply as strings.
- A loan can be recorded as a pair `(Person, Book)`.
- Deviating from the book, we can consider a library database as a triple consisting of:
    - A list of all members.
    - A list of all available books. We assume that the library has multiple copies of the same book, even if it only appears once on the list.
    - A list of book loans.

    The overall type would be: `([Person], [Book], [(Person, Book)])`

```haskell
type Person = String
type Book = String
type Library = ([Person], [Book], [(Person, Book)])
```

Always think of using functions you already created when solving a question.

1. Write an example database, that includes some checked out books, some books that are not checked out, as well as some members that have no books checked out. Store it in a variable `exampleBase`.
2. Write a function that given a library returns all the books that are checked out.
3. Write a function that given a library returns all the persons that have books checked out.
4. Write a function that given a library and a person returns the list of all books that person has checked out.
5. Write a function that given a library and a book returns the list of all persons that have the book checked out.
6. Write a function that given a library and a book returns whether the book is checked out.
7. Write a function that given a library and a person returns the number of books that person has checked out.
8. Write a function that given a library returns a list of pairs, each pair consisting with a person and the number of books they have checked out (perhaps 0).
9. Write a function that "checks out" a book as follows: Given a library, a person and a book, it returns a new library which contains all the previous information but also contains a new loan entry for that person-book pair.
10. Write a function that "returns" a book as follows: Given a library, a person and a book, it returns a new library where there is no loan entry for that person/book combination.
11. Write a function that given a library returns a list of all the books that have not been checked out.
12. Write a function that given a library returns a list of all members that have no books checked out.
