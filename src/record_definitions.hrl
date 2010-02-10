-record(person, {'Idno', 'FirstName', 'LastName', 'Dob', 'Ssn'}). 
-define(personFields, record_info(fields, person)).

-record(employer, {'Id', 'Name', 'Address'}). 
-define(employerFields, record_info(fields, employer)).

-record(paycheck, {'Id', 'CheckNumber'}). 
-define(paycheckFields, record_info(fields, paycheck)).

-define(recordMetadata, 
    [{person, record_info(fields, person)},
	{employer, record_info(fields, employer)},
	{paycheck, record_info(fields, paycheck)}].