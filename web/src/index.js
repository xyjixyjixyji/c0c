import wabt from "wabt";
import init, { compile_to_wasm, compile_to_x86 } from "../pkg/compile_wasm.js";

init();

function loadAndInstantiateWAT() {
    return fetch("./dist/alloc.wat")
        .then((response) => {
            if (!response.ok) {
                throw new Error(
                    "Network response was not ok " + response.statusText
                );
            }
            return response.text();
        })
        .then((watText) => {
            return wabt().then((wabtInstance) => {
                const parsedModule = wabtInstance.parseWat("", watText);
                const binaryModule = parsedModule.toBinary({});
                return WebAssembly.instantiate(binaryModule.buffer);
            });
        })
        .catch((error) => {
            console.error(
                "Error in loading or instantiating the wat file:",
                error
            );
        });
}

function handleSubmit() {
    const input = document.getElementById("edit-container").value;
    const output = compile_to_wasm("", input);
    const x64_output = compile_to_x86("", input);
    loadAndInstantiateWAT().then((allocModule) => {
        document.getElementById("wasm").textContent = output;
        wabt().then((wabt) => {
            const module = wabt.parseWat("", output).toBinary({}).buffer;

            const imports = {
                alloc: {
                    allocate: allocModule.instance.exports.allocate,
                    memory: allocModule.instance.exports.memory,
                },
            };

            WebAssembly.instantiate(module, imports).then((result) => {
                const final_result = result.instance.exports.main();
                document.getElementById("result").textContent = final_result;
            });
        });
    });
    document.getElementById("x86").textContent = x64_output;
}

document.addEventListener("DOMContentLoaded", function () {
    const submitButton = document.getElementById("compile-button");
    submitButton.addEventListener("click", handleSubmit);
});
