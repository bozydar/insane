import os
import re


class Processor:
    DOT_INSERT = re.compile(r"```dot-compile(?P<opts>.*?)\n(?P<code>.*?)```", re.MULTILINE | re.DOTALL)

    def __init__(self, files: [str], output: str = None):
        self.files = files
        self.output = output or "README.md"

    def proceed(self):
        with open(self.output, "w") as output_file:
            for file in self.files:
                with open(file) as file_:
                    content = file_.read()
                    content = Processor.DOT_INSERT.sub(self.replace, content)
                    output_file.write(content)

    def replace(self, m):
        cd = CompileDot(m['code'], **self.parse_opts(m['opts']))
        file_name = cd.proceed()
        return f"![](./{file_name})"

    @staticmethod
    def parse_opts(opts: str):
        return eval(opts)


class CompileDot:
    def __init__(self, code: str, **kwargs):
        self.code = code
        self.file_name = kwargs.get('name') or "tmp-" + str(abs(self.code.__hash__()))
        self.file_name = self._normalize_name(self.file_name)

    def proceed(self) -> str:
        with open(f"{self.file_name}.dot", "w") as file:
            file.write(self.code)

        output = self.output_file_name()
        os.system(f"dot {self.file_name}.dot -Tpng -o {output}")
        return output

    def output_file_name(self) -> str:
        return f"{self.file_name}.png"

    def _normalize_name(self, name: str) -> str:
        return re.sub(r'\W', '-', name).lower()


processor = Processor(['modules.md'])
processor.proceed()
