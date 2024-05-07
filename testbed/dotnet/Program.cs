using System.CommandLine;

namespace mxssy;

class Program : IDisposable
{
    static void Main(string[] args)
    {
        var sanitizerOption = new Option<string>
            ("--sanitizer", "The sanitizer to use");

        var rootCommand = new RootCommand { sanitizerOption };

        rootCommand.SetHandler((sanitizerOptionValue) =>
            {
                using var p = new Program();
                p.Run(sanitizerOptionValue);
            },
            sanitizerOption);
        rootCommand.Invoke(args);
    }

    private readonly Dictionary<string, ISanitizer> _sanitizers;
    private readonly Database _database;

    private Program()
    {
        var sanitizers = new ISanitizer[]
        {
            new GanssSanitizer(),
            new HtmlRule(),
            new GanssSanitizerLax(),
            new HtmlRuleLax()
        };
        this._sanitizers = sanitizers.ToDictionary(sanitizer => sanitizer.name);
        this._database = new Database();
    }

    private void Run(string sanitizerName)
    {
        var sanitizer = this._sanitizers[sanitizerName];
        var sanitizerId = this._database.GetSanitizerId(sanitizerName);
        while (true)
        {
            var fragments = this._database.GetNextBatch(sanitizerId);
            if (fragments.Count == 0)
            {
                Console.WriteLine("Waiting for new fragments to sanitize...");
                Thread.Sleep(5 * 60 * 1000);
            }
            ProcessBatch(sanitizerId, sanitizer, fragments);
        }
    }

    private void ProcessBatch(int sanitizerId, ISanitizer sanitizer, List<Fragment> fragments)
    {
        foreach (var fragment in fragments)
        {
            var errored = false;
            var errorMessage = "";
            SanitizerResult? output = null;
            try
            {
                output = sanitizer.sanitize(fragment.Payload);
            }
            catch (Exception ex)
            {
                errored = true;
                errorMessage = ex.Message;
            }

            if (output != null)
            {
                //Console.WriteLine($"{sanitizer.name}: {fragment.Payload} -> {output.Clean} ({output.Serialized}");
                this._database.AddSanitizerResult(fragment.Id, sanitizerId, output, errored, errorMessage, fragment.TsId);
            }
        }
    }

    public void Dispose()
    {
        _database.Dispose();
    }
}
