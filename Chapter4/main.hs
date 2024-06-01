


-- LHS = name of type, RHS = normal (value level) function. This == punning
data CustomerInfo = CustomerInfo String String Int Int


-- CustomerInfo getters

firstName :: CustomerInfo -> String
firstName (CustomerInfo fn _ _ _) = fn

lastName :: CustomerInfo -> String
lastName (CustomerInfo _ ln _ _) = ln

widgetCount :: CustomerInfo -> Int
widgetCount (CustomerInfo _ _ count _) = count

balance :: CustomerInfo -> Int
balance (CustomerInfo _ _ _ b) = b


-- setters

updateFirstName :: CustomerInfo -> String -> CustomerInfo
updateFirstName (CustomerInfo _ ln count balance) firstName = CustomerInfo firstName ln count balance

updateLastName :: CustomerInfo -> String -> CustomerInfo
updateLastName (CustomerInfo fn _ count balance) lastName = CustomerInfo fn lastName count balance

updateCount :: CustomerInfo -> Int -> CustomerInfo
updateCount (CustomerInfo fn ln _ balance) count = CustomerInfo fn ln count balance

updateBalance :: CustomerInfo -> Int -> CustomerInfo
updateBalance (CustomerInfo fn ln count _) balance = CustomerInfo fn ln count balance

-- example CustomerInfo records 

customerGeorge :: CustomerInfo
customerGeorge = CustomerInfo "Georgie" "Bird" 10 100


-- functions on CustomerInfo 

showCustomer :: CustomerInfo -> String
showCustomer (CustomerInfo first last count balance) = 
    let fullName = first <> " " <> last
        name = "name: " <> fullName  
        count' = "count: " <> (show count)
        balance' = "balance: " <> (show balance)
    in name <> " " <> count' <> " " <> balance'


applyDiscount :: CustomerInfo -> CustomerInfo
applyDiscount customer = 
    case customer of 
        (CustomerInfo "Georgie" "Bird" count balance) -> CustomerInfo "Georgie" "Bird" count (div balance 4)
        (CustomerInfo "Porter" "Pupper" count balance) -> CustomerInfo "Porter" "Pupper" count (div balance 2)
        otherCustomer -> otherCustomer

