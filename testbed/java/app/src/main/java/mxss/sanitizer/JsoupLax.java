package mxss.sanitizer;

import mxss.Result;
import org.jsoup.safety.Safelist;

public class JsoupLax extends JsoupSanitizer implements Sanitizer {


    @Override
    public Result sanitize(String input) {
        return super.sanitize(input, Safelist.relaxed().addTags(TAGS).addAttributes(":all", "id", "data-foo", "title", "class", "name"));
    }

    @Override
    public String getName() {
        return "jsoup-lax";
    }
}
