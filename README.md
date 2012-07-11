hs-task
=======

`hs-task` is an in-code task management utility.

Many IDEs and editors contain some form of built-in task management capabilities.
Phylosophy behind `hs-task` is to provide a standlone, in-code, task management utility that can be interfaced via command-line, IDEs and editors.

`hs-task` icludes the command-line utilities `task-find` and `task-crunch` (see below for usage).

For IDE/editor integration, the Eclipse sample plugin can be located [here](https://github.com/billdozr/com.alenribic.atodo).

**Note** the project is still in its infancy. Comments and feedback are more than welcome!

usage:
=============================

task-find [OPTION...] toplevel_dir|file_path

	-R	Recurse sub-directories

	-f	Filter by file extention

	-h	This usage info

task-crunch [OPTION...] raw_task

	-o	Output format (plain | html | xml)

	-h	This usage info

example:
-----------------

To find all tasks (currently TODO tasks only) in python source files and then output them as plain text run
> task-find -R -f .py ~/Development/python/projects/ | task-crunch

Result
    @TODO(bug, mlcomp, #alen, #bob): File path iteration fails on deep recursion | added a test case | H, 6 | "/Users/alen/Development/python/billdozr_env/projects/BilldozrML/src/parser/load_data.py", (14,1), Wed Jul 11 13:23:27 SAST 2012
    ...

And for html output run
> task-find -R -f .py ~/Development/python/projects/ | task-crunch -o html > /tmp/task-sample.html

Result
		List of Todo Task's:

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

The cool thing here is that we can use the xml supported output which then can be pulled into any Editor/IDE we can build a plugin for.

**See:** the Eclipse [plugin sample](https://github.com/billdozr/com.alenribic.atodo) I built (barely alpha). 

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
