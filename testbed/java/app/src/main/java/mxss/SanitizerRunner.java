package mxss;

import mxss.sanitizer.Sanitizer;

import java.util.List;


public class SanitizerRunner implements AutoCloseable {

    private final Database database;
    private final Sanitizer sanitizer;
    public SanitizerRunner(Database database, Sanitizer sanitizer) {
        this.database = database;
        this.sanitizer = sanitizer;
    }

    public void run() throws Exception {
        int sanitizerId = this.database.getSanitizerId(this.sanitizer.getName());
        System.out.printf("Running with sanitizer id: %d%n", sanitizerId);
        while(true) {
            List<CodeFragment> fragments = this.database.getNextBatch(sanitizerId);
            if(fragments.isEmpty()) {
                System.out.println("Waiting for new fragments!");
                Thread.sleep(50000L);
                continue;
            }
            for(CodeFragment cf : fragments) {
                boolean errored = false;
                String errorMessage = null;
                Result res = null;
                try {
                     res = this.sanitizer.sanitize(cf.getPayload());
                } catch(Exception ex) {
                    errored = true;
                    errorMessage = ex.getMessage();
                }
                String sanitized = res != null ? res.getCleaned() : "";
                String serialized = res != null ? res.getSerialized() : null;
                boolean done = sanitized.contains("mxss");
                this.database.addSanitizerResult(cf.getId(), sanitizerId, sanitized, serialized,  errored, errorMessage, done, cf.getTsId());

            }
        }
    }

    @Override
    public void close() throws Exception {
        this.database.close();
    }
}
