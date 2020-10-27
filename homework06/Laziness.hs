--Strict Evaluation
-- The opposite of lazy evaluation -> stric evaluation.
-- The function arguments are completely evaluated before passing them to the function
-- under a stric evaluation strategy.
f x y = x + 2
-- The benefit of strict evaluation is we can predict when and in which order things will happen.
-- In Java example:
-- f(release_monkeys(), increment_counter())
-- mokeys will be released, and the couter will be incremented, and then the results of doing those things
-- will be passed to f, and it does not matter whether f actually ends up using those results.
-- But if the order depends on whether f happends to use their results, it would be extremly confusing.

-- Side Effects and Purity
-- By "side effect" we mean anything that causes evaluation of an expression to interact with something outside itself.
-- The main issue is that such outside interactions are time-sensitive.
-- Such that: modifying a global variable. -- it may affect the evaluation of other expressions.
-- Printing to the screen: it may need to be in a certain order with respect to other writers to the screen.
-- Reaing from file or the network. -- The contents of the file can affect the outcome of the expression.
-- So the side effects in a lazy language would be extremly untuitive.
-- That's why the designer of Haskell makes it pure.

