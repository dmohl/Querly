-record(person, {idno, firstName, lastName, dob, ssn}). 
-define(personFields, record_info(fields, person)).

-record(employer, {id, name, address}). 
-define(employerFields, record_info(fields, employer)).

-record(paycheck, {id, checkNumber}). 
-define(paycheckFields, record_info(fields, paycheck)).

-define(recordMetadata, 
    [{person, record_info(fields, person)},
	{employer, record_info(fields, employer)},
	{paycheck, record_info(fields, paycheck)}].