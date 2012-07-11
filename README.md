hs-todo
=======

Lightweight TODO code-level manager.

Usage:
=============================

todo-crunch -h
todo-crunch: Usage: todo-crunch [OPTION...] raw_todo

	-o	Output format (plain | html | xml)

	-h	This usage info

todo-find -h
todo-find: Usage: todo-find [OPTION...] toplevel_dir|file_path

	-R	Recurse sub-directories

	-f	Filter by file extention

	-h	This usage info


Shell prompt:
-----------------

To find all TODO tasks in python source files and then output them as html
> todo-find -R -f .py ~/Development/python/projects/ | todo-crunch -o html > /tmp/todo-sample.html

The cool thing here is that oen can use the xml supported output which then can be pulled into any IDE we can build a plugin for.
**See:** the Eclipse plugin sample I built (very alpha). 

License:
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
