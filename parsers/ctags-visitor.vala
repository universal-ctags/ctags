/*
 * ctags-visitor.vala
 *
 * Copyright 2008, 2010 Abderrahim Kitouni <a.kitouni@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301, USA.
 */

using Vala;

enum ValaKind {
	K_CLASS,
	K_STRUCT,
	K_INTERFACE,
	K_ENUM,
	K_ENUMVALUE,
	K_ERRORDOMAIN,
	K_ERRORCODE,
	K_DELEGATE,
	K_SIGNAL,
	K_FIELD,
	K_PROPERTY,
	K_METHOD,
	K_LOCAL,
	COUNT_KIND
}

public struct CTagsEntry {
	public int line_number;
	public string name;
	public int kind;
	public size_t filePosition;
	public string access;
	public string implementation;
	public string inheritance;
	public string scope[2];
	public string signature;
	public string typeref;
	public string returntype;
}

class DummyReport : Report {
	public override void warn (SourceReference? source, string message) {}
	public override void err (SourceReference? source, string message) {}
}

[CCode (has_target=false)]
public delegate void CTagsEntryMaker (CTagsEntry entry);

public class CTagsVisitor : CodeVisitor {
	Parser vala_parser;
	Genie.Parser genie_parser;
	GLib.List<CTagsEntry?> taglist;
	public CTagsVisitor () {
		vala_parser = new Parser();
		genie_parser = new Genie.Parser();
	}
	/* helper functions */
	static string get_access (Symbol sym) {
		switch (sym.access) {
		case SymbolAccessibility.PRIVATE : return "private";
		case SymbolAccessibility.INTERNAL : return "internal";
		case SymbolAccessibility.PROTECTED : return "protected";
		case SymbolAccessibility.PUBLIC : return "public";
		}
		assert_not_reached ();
	}
	static string to_string (Iterable<DataType> seq, string sep = ",") {
		var str = new StringBuilder();
		var first = true;
		foreach (var type in seq) {
			if(first) {
				first = false;
			} else {
				str.append(sep);
			}
			str.append(type.to_qualified_string());
		}
		return str.str;
	}
	static string? implementation (Symbol sym) {
		var list = new GLib.List<string>();

		if (sym is Vala.Signal) {
			var sig = (Vala.Signal)sym;
			if (sig.is_virtual)
				list.append("virtual");
		} else if (sym is Class) {
			var cls = (Class)sym;
			if (cls.is_abstract)
				list.append("abstract");
		} else if (sym is Method) {
			var meth = (Method)sym;
			if (meth.is_abstract)
				list.append("abstract");
			else if (meth.is_virtual)
				list.append("virtual");
		} else if (sym is Property) {
			var prop = (Property)sym;
			if (prop.is_abstract)
				list.append("abstract");
			else if (prop.is_virtual)
				list.append("virtual");
		} else
			return_val_if_reached(null);

		var ret = new StringBuilder();
		var first = true;
		foreach (var str in list) {
			if(first) {
				first = false;
			} else {
				ret.append(",");
			}
			ret.append(str);
		}
		return ret.str;
	}
	static string signature (Vala.List<Vala.Parameter> parameter) {
		var ret = new StringBuilder("(");
		var first = true;
		foreach (var p in parameter) {
			if(first) {
				first = false;
			} else {
				ret.append(",");
			}
			if (p.ellipsis) {
				ret.append("...");
			} else {
				ret.append (p.variable_type.to_qualified_string());
				ret.append (" ");
				ret.append (p.name);
			}
		}
		ret.append (")");
		return ret.str;
	}
	static string[] scope (Symbol s) {
		string scope[2];
		var par = s.parent_symbol;
		if (par != null && par.name != null) {
			if (par is Class)
				scope[0] = "class";
			else if (par is Struct)
				scope[0] = "struct";
			else if (par is Interface)
				scope[0] = "interface";
			else
				return scope;
			scope[1] = par.name;
		}
		return scope;
	}

	public override void visit_source_file (SourceFile source_file) {
		source_file.accept_children (this);
	}

	public override void visit_class (Class cl) {
		var entry = CTagsEntry();

		entry.line_number = cl.source_reference.begin.line;
		entry.name = cl.name;
		entry.kind = ValaKind.K_CLASS;
		entry.filePosition = cl.source_reference.begin.pos
				   - cl.source_reference.file.get_mapped_contents();
		entry.access = get_access (cl);
		entry.implementation = implementation(cl);
		entry.inheritance = to_string(cl.get_base_types(), ",");
		entry.scope = scope (cl);

		taglist.append(entry);
		cl.accept_children(this);
	}
	public override void visit_struct (Struct st) {
		var entry = CTagsEntry();
		entry.line_number = st.source_reference.begin.line;
		entry.name = st.name;
		entry.kind = ValaKind.K_STRUCT;
		entry.filePosition = st.source_reference.begin.pos
				   - st.source_reference.file.get_mapped_contents();
		entry.access = get_access (st);
		entry.scope = scope (st);

		taglist.append(entry);
		st.accept_children(this);
	}
	public override void visit_interface (Interface iface) {
		var entry = CTagsEntry();

		entry.line_number = iface.source_reference.begin.line;
		entry.name = iface.name;
		entry.kind = ValaKind.K_INTERFACE;
		entry.filePosition = iface.source_reference.begin.pos
				   - iface.source_reference.file.get_mapped_contents();
		entry.access = get_access (iface);
		entry.inheritance = to_string(iface.get_prerequisites());
		entry.scope = scope (iface);

		taglist.append(entry);
		iface.accept_children(this);
	}

	public override void visit_enum (Vala.Enum en) {
		var entry = CTagsEntry();

		entry.line_number = en.source_reference.begin.line;
		entry.name = en.name;
		entry.kind = ValaKind.K_ENUMVALUE;
		entry.filePosition = en.source_reference.begin.pos
				   - en.source_reference.file.get_mapped_contents();
		entry.access = get_access (en);
		entry.scope = scope (en);

		taglist.append(entry);
		en.accept_children(this);
	}
	public override void visit_error_domain (ErrorDomain edomain) {
		var entry = CTagsEntry();

		entry.line_number = edomain.source_reference.begin.line;
		entry.name = edomain.name;
		entry.kind = ValaKind.K_ERRORDOMAIN;
		entry.filePosition = edomain.source_reference.begin.pos
				   - edomain.source_reference.file.get_mapped_contents();
		entry.access = get_access (edomain);
		entry.scope = scope (edomain);

		taglist.append(entry);
		edomain.accept_children(this);
	}

	public override void visit_enum_value (Vala.EnumValue ev) {
		var entry = CTagsEntry();

		entry.line_number = ev.source_reference.begin.line;
		entry.name = ev.name;
		entry.kind = ValaKind.K_ENUMVALUE;
		entry.filePosition = ev.source_reference.begin.pos
				   - ev.source_reference.file.get_mapped_contents();
		entry.access = get_access (ev);
		entry.scope = scope (ev);

		taglist.append(entry);
	}
	public override void visit_error_code (ErrorCode ecode) {
		var entry = CTagsEntry();

		//entry.line_number = ecode.source_reference.begin.line;
		entry.name = ecode.name;
		entry.kind = ValaKind.K_ERRORCODE;
		entry.filePosition = ecode.source_reference.begin.pos
				   - ecode.source_reference.file.get_mapped_contents();
		entry.access = get_access (ecode);
		entry.scope = scope (ecode);

		taglist.append(entry);
	}

	public override void visit_delegate (Delegate d) {
		var entry = CTagsEntry();

		entry.line_number = d.source_reference.begin.line;
		entry.name = d.name;
		entry.kind = ValaKind.K_DELEGATE;
		entry.filePosition = d.source_reference.begin.pos
				   - d.source_reference.file.get_mapped_contents();
		entry.access = get_access (d);
		entry.scope = scope (d);
		entry.returntype = d.return_type.to_qualified_string();
		entry.signature = signature(d.get_parameters());

		taglist.append(entry);
	}
	public override void visit_signal (Vala.Signal sig) {
		var entry = CTagsEntry();

		entry.line_number = sig.source_reference.begin.line;
		entry.name = sig.name;
		entry.kind = ValaKind.K_SIGNAL;
		entry.filePosition = sig.source_reference.begin.pos
				   - sig.source_reference.file.get_mapped_contents();
		entry.access = get_access (sig);
		entry.implementation = implementation(sig);
		entry.scope = scope (sig);
		entry.returntype = sig.return_type.to_qualified_string();
		entry.signature = signature(sig.get_parameters());

		taglist.append(entry);
	}
	public override void visit_field (Field f) {
		var entry = CTagsEntry();

		entry.line_number = f.source_reference.begin.line;
		entry.name = f.name;
		entry.kind = ValaKind.K_FIELD;
		entry.filePosition = f.source_reference.begin.pos
				   - f.source_reference.file.get_mapped_contents();
		entry.access = get_access (f);
		entry.scope = scope (f);
		entry.typeref = f.variable_type.to_qualified_string();

		taglist.append(entry);
	}
	public override void visit_constant (Constant c) {
		var entry = CTagsEntry();

		entry.line_number = c.source_reference.begin.line;
		entry.name = c.name;
		entry.kind = ValaKind.K_FIELD;
		entry.filePosition = c.source_reference.begin.pos
				   - c.source_reference.file.get_mapped_contents();
		entry.access = get_access (c);
		entry.scope = scope (c);
		entry.typeref = c.type_reference.to_qualified_string();
  
		taglist.append(entry);
	}
	public override void visit_property (Property prop) {
		var entry = CTagsEntry();

		entry.line_number = prop.source_reference.begin.line;
		entry.name = prop.name;
		entry.kind = ValaKind.K_PROPERTY;
		entry.filePosition = prop.source_reference.begin.pos
				   - prop.source_reference.file.get_mapped_contents();
		entry.access = get_access (prop);
		entry.implementation = implementation(prop);
		entry.scope = scope (prop);
		entry.typeref = prop.property_type.to_qualified_string();

		taglist.append(entry);
	}

	public override void visit_method (Method m) {
		var entry = CTagsEntry();

		entry.line_number = m.source_reference.begin.line;
		entry.name = m.name;
		entry.kind = ValaKind.K_METHOD;
		entry.filePosition = m.source_reference.begin.pos
				   - m.source_reference.file.get_mapped_contents();
		entry.access = get_access (m);
		entry.implementation = implementation(m);
		entry.scope = scope (m);
		entry.returntype = m.return_type.to_qualified_string();
		entry.signature = signature(m.get_parameters());

		taglist.append(entry);
	}

	public override void visit_local_variable (LocalVariable local) {
		var entry = CTagsEntry();

		entry.line_number = local.source_reference.begin.line;
		entry.name = local.name;
		entry.kind = ValaKind.K_LOCAL;
		entry.filePosition = local.source_reference.begin.pos
				   - local.source_reference.file.get_mapped_contents();
		entry.access = get_access (local);

		taglist.append(entry);
	}

	public void parse_vala (string filename, CTagsEntryMaker maker ) {
		taglist = new GLib.List<CTagsEntry?>();
		/* We create a context for every source file so that Parser.parse(context)
		 * don't parse a file multiple times causing errors. Parser.parse_file(source_file)
		 * assumes that Parser.context is the same as source_file.context anyway */
		var context = new CodeContext();
		context.report = new DummyReport();
		var source_file = new SourceFile(context, filename.has_suffix("vapi") ? SourceFileType.PACKAGE : SourceFileType.SOURCE, filename);

		CodeContext.push(context);
		context.add_source_file(source_file);
		vala_parser.parse(context);
		context.accept(this);
		foreach (var tagentry in taglist) {
			maker(tagentry);
		}
		taglist = null;
		CodeContext.pop();
	}
	public void parse_genie (string filename, CTagsEntryMaker maker ) {
		taglist = new GLib.List<CTagsEntry?>();
		var context = new CodeContext();
		context.report = new DummyReport();
		var source_file = new SourceFile(context, SourceFileType.SOURCE, filename);
		context.add_source_file(source_file);

		CodeContext.push(context);
		genie_parser.parse(context);
		context.accept(this);
		foreach (var tagentry in taglist) {
			maker(tagentry);
		}
		taglist = null;
		CodeContext.pop();
	}
}
