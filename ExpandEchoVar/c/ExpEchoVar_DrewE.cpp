// Sat Jan 16 13:23:55 PST 2016
#include <cassert> 
#include <iostream>
#include <string>
 
static int
adjust_depth(int in, char c)
{
    return c == '{' ? in + 1 : c == '}' ? in - 1 : in;
}
 
std::string
expand(const std::string &in)
{
    std::string::const_iterator options_start;
    std::string ret;
 
    for (options_start = in.begin();
         options_start != in.end() && *options_start != '{';
         ++options_start) {
    }
    const std::string prefix(in.begin(), options_start);
 
    if (options_start == in.end()) {
        ret = prefix;
    } else {
        std::string::const_iterator options_end;
        int depth;
 
        for (options_end = options_start + 1, depth=0;
             options_end != in.end() &&
                 (*options_end != '}' || depth);
             ++options_end) {
            depth = adjust_depth(depth, *options_end);
        }
        const std::string suffix(options_end + 1, in.end());
 
        std::string::const_iterator option_begin, option_end;
 
        for (option_begin = options_start + 1;
             option_begin != options_end;) {
            for (option_end = option_begin, depth = 0;
                 option_end != options_end &&
                     (*option_end != ',' || depth);
                 ++option_end) {
                depth = adjust_depth(depth, *option_end);
            }
            const std::string expanded(option_begin, option_end);
 
            if (option_begin != options_start + 1) {
                ret += ' '; 
            }
            ret += expand(prefix + expanded + suffix);
 
            option_begin = option_end;
            if (option_begin != options_end) {
                ++option_begin;
            }
        }
    }
 
    return ret;
}
 
static void
test(const std::string &example, const std::string &expect)
{
    const std::string got(expand(example)); 
    std::cout << example << ' ' << got << std::endl;
    assert(got == expect);
}
 
int
main()
{
    test("{{b,c},{e,f}}{g,h}", "beg beh bfg bfh ceg ceh ceg cfh");
    test("abc", "abc");
    test("{a,b}{c,d}", "ac ad bc bd");
    test("{a,b}{c,g{e,m}}p{q,r}",
         "acpq acpr agepq agepr agmpq agmpr bcpq bcpr bgepq bgepr bgmpq bgmpr");
    return 0;
}
// Sat Jan 16 13:45:41 PST 2016 code complete
