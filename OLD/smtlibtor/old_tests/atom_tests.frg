#lang forge

option backend smtlibtor 

// option run_sterling off
// option verbose 10

sig Person {
    parent : lone Person,
    age : one Int,
    friends : one Int
}

sig Animal {
    owner : lone Person
}


run {some p : Person | p.age > 10 and p.friends < 18}


