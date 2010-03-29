using System;
using Symbiote.Core.Extensions;
using Symbiote.Core.Log;
using Symbiote.Daemon;
using Symbiote.Jackalope;
using Symbiote.Relax;

namespace QuerlyDemo
{
    public class QuerlyDemo : IDaemon
    {
        private ILogger<QuerlyDemo> _logger;
        private IBus _bus;
        private IDocumentRepository _documents;

        public void Start()
        {
            _documents.GetAll<ExchangeRecord>();
            CreateMessages();
            Stop();
        }

        public void Stop()
        {
            "Messages created.  Goodbye.".ToInfo<QuerlyDemo>();
        }

        private void CreateMessages()
        {
            "Creating messages...".ToInfo<QuerlyDemo>();

            TestBus(30);
        }

        private void TestBus(int iterations)
        {
            for (int index = 0; index < iterations; index++)
            {
                var person = Person.Create(10000 + index, "test First", "Test Last",
                                           DateTime.Now, "123-12-1233");
                var querlyTicket = QuerlyTicket.Create("person",
                                    person.Id.ToString(), "add")
                    .ToJson();
                var querlyTicketJsonDocument = new JsonDocument
                                       {
                                           Body = querlyTicket
                                       };
                Console.WriteLine(querlyTicketJsonDocument.Body);
                _bus.Send("querlydemo", querlyTicketJsonDocument);
                _documents.Save(person);
            }
        }

        public QuerlyDemo(ILogger<QuerlyDemo> logger, IBus bus, 
            IDocumentRepository documents)
        {
            _logger = logger;
            _bus = bus;
            _documents = documents;

            _bus.SetupEndPoint("querlydemo", "querlydemo", ExchangeType.topic);
        }
    }

    public class ExchangeRecord : DefaultCouchDocument
    {
        public string ExchangeName { get; set; }
    }
}