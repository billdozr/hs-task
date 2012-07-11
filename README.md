hs-todo
=======

Lightweight TODO code-level manager.

usage:
=============================

todo-crunch: Usage: todo-crunch [OPTION...] raw_todo

	-o	Output format (plain | html | xml)

	-h	This usage info

todo-find: Usage: todo-find [OPTION...] toplevel_dir|file_path

	-R	Recurse sub-directories

	-f	Filter by file extention

	-h	This usage info

example:
-----------------

To find all TODO tasks in python source files and then output them as plain text
> todo-find -R -f .py ~/Development/python/projects/ | todo-crunch

    @TODO(bug, mlcomp, #alen, #bob): File path iteration fails on deep recursion | added a test case | H, 6 | "/Users/alen/Development/python/billdozr_env/projects/BilldozrML/src/parser/load_data.py", (14,1), Wed Jul 11 13:23:27 SAST 2012
    ...

Here is an output with html
> todo-find -R -f .py ~/Development/python/projects/ | todo-crunch -o html > /tmp/todo-sample.html

    A list of Todo's:

		Subject: File path iteration fails on deep recursion
		Action: added a test case
		Label(s): bug, mlcomp
		User(s): alen, bob
		Priority: High
		Time spent: 6
		Source file: /Users/alen/Development/python/billdozr_env/projects/BilldozrML/src/parser/load_data.py
		Line / Column: (14,1)
		File modified: Wed Jul 11 13:23:27 SAST 2012

		...

The cool thing here is that oen can use the xml supported output which then can be pulled into any IDE we can build a plugin for.

**See:** the Eclipse plugin sample I built (very alpha). 

license:
=============================

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
