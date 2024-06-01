{-# LANGUAGE RecordWildCards #-}

data CustomerInfo = CustomerInfo {
    customerInfoFirstName :: String,
    customerInfoLastName :: String,
    customerInfoWidgetCount :: Int,
    customerInfoBalance :: Int
}


data EmployeeInfo = CustomerInfo {
    employeeInfoFirstName :: String,
    employeeInfoLastName :: String,
    employeeInfoTimezone :: String,
    employeeInfoContactInfo :: String
}

-- use "fully qualified names" in order to disambiguate duplicate record fields..
-- personal opinion: not super clean, but seems to be somewhat conventional.
