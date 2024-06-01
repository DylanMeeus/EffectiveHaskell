-- sum types

data Bool = True | False

data Direction = North | East | South | West

data PreferredContactMethod = Email String | TextMessage String | SnailMail String String String Int


emailContact :: PreferredContactMethod 
emailContact = Email "me@example.com"

textContact :: PreferredContactMethod
textContact = TextMessage "0118 999 881 999 119 725 3"

mailContact :: PreferredContactMethod
mailContact = SnailMail "123 someStreet" "Suite 3108" "ExampleVille" 8123

confirmContact :: PreferredContactMethod -> String
confirmContact contact =
    case contact of
        Email e -> "Okay, I'll email you at: " <> e
        TextMessage tm -> "Okay, I'll text you at: " <> tm 
        SnailMail s1 s2 city zip -> "Okay, I'll send a letter to: " <>  formatWithSpaces [s1, s2, city, show zip]


confirmContactHidden :: PreferredContactMethod -> String
confirmContactHidden contact =
    case contact of
        Email{} -> "Okay, I'll email you" 
        TextMessage{} -> "Okay, I'll text you" 
        SnailMail{} -> "Okay, I'll send a letter" 


formatWithSpaces :: [String] -> String
formatWithSpaces input = foldl1 (\x y -> x <> " " <> y) input


-- risk with sum type of records, is that not all fields that exist for one inhabitant exist for the
-- other (e.g, below 'balance' only exists for Customer, but not Employee). This will cause an
-- exception at runtime.
-- To solve this, use a sum type of two _distinct_ record types
-- data Person = Customer { name :: String, balance :: Int } | Employee { name :: String, managerName :: String, salary :: Int }

--george :: Person
--george = Customer { name = "George", balance = 100 }

--porter :: Person
--porter = Employee { name = "Porter", managerName = "Remi", salary = 10 }

-- example of using sum type of distinct record types
data CustomerInfo = CustomerInfo { customerName :: String, customerBalance :: Int }
data EmployeeInfo = EmployeeInfo { employeeName :: String, employeeManagerName :: String, employeeSalary :: Int }

data StorePerson = Customer CustomerInfo | Employee EmployeeInfo

data MaybeString = NoString | SomeString String

georgeInfo = Customer $ CustomerInfo { customerName = "george", customerBalance = 100 }
porterInfo = Employee $ EmployeeInfo { employeeName = "porter", employeeManagerName = "remi", employeeSalary = 400 }

getPersonName :: StorePerson -> String
getPersonName person = 
    case person of
        Employee e -> employeeName e
        Customer c -> customerName c

getPersonManager :: StorePerson -> MaybeString
getPersonManager person =
    case person of
        Employee e -> SomeString ( employeeManagerName e )
        Customer c -> NoString


printMaybeString :: MaybeString -> String
printMaybeString maybe = 
    case maybe of 
        NoString -> "Nothing to see here, move along.."
        SomeString s -> s
