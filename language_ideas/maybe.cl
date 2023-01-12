
type Option(a) {
    Some(a)
    None
}


protocol Debug(this) {
    this.debug : String
}

protocol TypeName(t) {
    typeName : String
}

instance if Debug(a) => Debug(Option(a)) {
    debug = match this {
        Some(v) => "Some({v.debug()})"
    }
}

instance if TypeName(a) => TypeName(Option(a)) {
    typeName = "Option({a.typeName})"
}

type Person {
    name: String
    age: Integer
}

type Void(a) = a -> ()
type Method(a) = () -> a

pub methods Person {
    setName :: Void(String)
    setName name {
        this.name = name 
    }

    setBetterName :: Void((String, String))
    setBetterName(name1, name2) {
        this.name = if name1.len > name2.len {
            name1 
        } else { 
            name 2
        }
    }

}

protocol Adult(t) {
    t.isAdult: Bool 
}

instance Adult(Person) {
    isAdult = age >= 18
}

// freestanding Function
isAdult :: Adult(t) => t -> Bool
isAdult(p) {
    p.isAdult
}



