package mxss.sanitizer;

import mxss.Result;
import org.jsoup.Jsoup;
import org.jsoup.safety.Safelist;

public class JsoupBasic extends JsoupSanitizer implements Sanitizer {


    @Override
    public Result sanitize(String input) {
        return super.sanitize(input, Safelist.basic());
    }

    @Override
    public String getName() {
        return "jsoup";
    }
}
