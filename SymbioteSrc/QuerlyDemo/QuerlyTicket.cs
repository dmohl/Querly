using System;

namespace QuerlyDemo
{
    [Serializable]
    public class QuerlyTicket
    {
        public virtual string DatabaseName { get; set; }
        public virtual string PrimaryKey { get; set; }
        public virtual string Action { get; set; }
        public virtual DateTime Created { get; set; }

        public static QuerlyTicket Create(string databaseName, string primaryKey, 
            string action)
        {
            return new QuerlyTicket()
                   {
                       DatabaseName = databaseName,
                       PrimaryKey = primaryKey,
                       Action = action,
                       Created = DateTime.Now
                   };
        }
    }
}
