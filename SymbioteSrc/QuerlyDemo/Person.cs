using System;
using Symbiote.Relax;

namespace QuerlyDemo
{
    public class Person : DefaultCouchDocument
    {
        public virtual int Idno { get; set; }
        public virtual string FirstName { get; set; }
        public virtual string LastName { get; set; }
        public virtual DateTime Dob { get; set; }
        public virtual string Ssn { get; set; }

        public static Person Create(int idno, string firstName, string lastName,
            DateTime dob, string ssn)
        {
            return new Person()
                   {
                       Idno = idno,
                       FirstName = firstName,
                       LastName = lastName,
                       Dob = dob,
                       Ssn = ssn
                   };
        }
    }
}