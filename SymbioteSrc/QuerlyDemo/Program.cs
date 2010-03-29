using Symbiote.Core;
using Symbiote.Daemon;
using Symbiote.Jackalope;
using Symbiote.Log4Net;
using Symbiote.Relax;

namespace QuerlyDemo
{
    class Program
    {
        static void Main(string[] args)
        {
            Assimilate
                .Core()
                .AddConsoleLogger<QuerlyDemo>(c => c.Info().MessageLayout(p => p.Date().Message().Newline()))
                .Daemon<QuerlyDemo>(x =>
                    x.Arguments(args)
                    .Name("QuerlyDemo")
                    .DisplayName("Querly Demo")
                    .Description("Publishes messages that are picked up by Querly"))
                .Relax(x => x.Server("localhost")
                                .UseForType<ExchangeRecord>("exchanges")
                                .UseForType<Person>("person"))
                                
                .Jackalope(x => x.AddServer(s => s.AMQP08().Address("localhost")))
                .RunDaemon<QuerlyDemo>();
        }
    }
}
