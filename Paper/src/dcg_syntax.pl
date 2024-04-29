head(X) --> [this], "is", [X], { word(X) }, !, [good, example], punct.
punct --> [!] | [?] | [.].
word(X) :- X = 'some'; X = 'a'.
