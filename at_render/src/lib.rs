
#[cfg(test)]
mod tests {
    use quick_js::{Context, JsValue};
    use std::fs::File;
    use rusty_v8 as v8;
    use std::io::Read;

    #[test]
    fn it_works() {
        let platform = v8::new_default_platform().unwrap();
        v8::V8::initialize_platform(platform);
        v8::V8::initialize();

        let script = Box::new({
            let mut script_file = File::open("dist/index.js").unwrap();
            let mut script = String::new();
            script_file.read_to_string(&mut script);
            script
        });

        let isolate = &mut v8::Isolate::new(Default::default());
        let scope = &mut v8::HandleScope::new(isolate);
        let context = v8::Context::new(scope);
        let scope = &mut v8::ContextScope::new(scope, context);

        let code = v8::String::new(scope, script.as_ref()).unwrap();
        let script = v8::Script::compile(scope, code, None).unwrap();

        script.run(scope).unwrap();
        let code = v8::String::new(scope, "someglobal.runSSR()").unwrap();
        let script = v8::Script::compile(scope, code, None).unwrap();

        let result = script.run(scope).unwrap();
        let result = result.to_string(scope).unwrap();

        println!("{:?}", result.to_rust_string_lossy(scope));
    }
}
