{-# LANGUAGE RecordWildCards #-}

--- record syntax now 

data CustomerInfo = CustomerInfo {
    firstName :: String,
    lastName :: String,
    widgetCount :: Int,
    balance :: Int
}

showCustomer :: CustomerInfo -> String
showCustomer CustomerInfo {..} = firstName <> " " <> lastName <> " " <> show widgetCount <> " " <> show  balance

customerGeorge :: CustomerInfo
customerGeorge = CustomerInfo {
        balance = 100,
        lastName = "Bird",
        firstName = "Georgie",
        widgetCount = 10
}

customerGeorge2 =
    let firstName = "Georgie"
        lastName = "Bird"
        widgetCount = 10
        balance = 100
    in CustomerInfo {..}

emptyCart :: CustomerInfo -> CustomerInfo
emptyCart c = c { widgetCount = 0, balance = 0 }

totalCount :: [CustomerInfo] -> Int
totalCount cs = sum $ map (\c -> widgetCount c) cs



