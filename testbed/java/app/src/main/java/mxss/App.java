package mxss;

import mxss.sanitizer.*;
import picocli.CommandLine;

import java.util.Map;
import java.util.concurrent.Callable;
import java.util.concurrent.ConcurrentHashMap;

@CommandLine.Command(
        description = "Runs Java Sanitizer against Payloads",
        name = "Java MXSS Testbed",
        mixinStandardHelpOptions = true,
        version = "0.0.1"
)
public class App implements Callable<Void> {
    @CommandLine.Option(
            names = "--host",
            required = true,
            paramLabel = "Database host",
            description = "Hostname of Database Server with port",
            defaultValue = "database:5432"
    )
    private String host;
    @CommandLine.Option(
            names = {"-u", "--username"},
            required = true,
            paramLabel = "Database user",
            description = "User to login into the database server",
            defaultValue = "mxssy"
    )
    private String username;
    @CommandLine.Option(
            names = {"-p", "--password"},
            required = true,
            paramLabel = "Database Password",
            description = "Password of the database user",
            defaultValue = "allyourmutationsarebelongtous"
    )
    private String password;
    @CommandLine.Option(
            names = {"-s", "--sanitizer"},
            paramLabel = "Sanitizer",
            description = "What sanitizer to use?",
            defaultValue = "jsoup"
    )
    private String sanitizer;
    public static void main(String[] args) {
        new CommandLine(new App())
                .setCaseInsensitiveEnumValuesAllowed(true)
                .execute(args);
    }

    private final Map<String, Sanitizer> runners = new ConcurrentHashMap<>();
    private void init() {
        Sanitizer[] sanitizers = {
                new JsoupBasic(),
                new JsoupLax(),
                new AntiSamy("antisamy"),
                new AntiSamy("antisamy-lax")
        };
        for(Sanitizer sanitizer : sanitizers) {
            this.runners.put(sanitizer.getName(), sanitizer);
        }
    }
    public void run() throws Exception {
        Sanitizer sanitizer = this.runners.get(this.sanitizer);
        if(sanitizer == null) {
            throw new IllegalArgumentException(String.format("Runner: '%s' does not exist!", this.sanitizer));
        }
        Database database = new Database(this.host, this.username, this.password);
        SanitizerRunner runner = new SanitizerRunner(database, sanitizer);
        runner.run();
    }

    @Override
    public Void call() throws Exception {
        init();
        run();
        return null;
    }
}
