package chee.util

import org.scalatest._
import mustache._
import fastparse.core.Parsed.Success

class MustacheTest extends FlatSpec with Matchers {

  "template" should "render literals" in {
    val t = Template(Literal("Hello"), Literal(" "), Literal("world!"))
    t.render(Context.empty) should be ("Hello world!")
  }

  it should "replace variables" in {
    val t = Template(Literal("Hello "), Variable("name"), Literal("!"))
    val context = Context("name" -> Value.of("Harry"))
    t.render(context) should be ("Hello Harry!")
  }

  it should "render nothing for non-existing vars" in {
    val t = Template(Literal("Hello "), Variable("name"), Literal("!"))
    t.render(Context.empty) should be ("Hello !")
  }

  it should "not render empty sections" in {
    val t = Template(Literal("Hello "), Section("name", Seq(Literal("World!"))))
    t.render(Context.empty) should be ("Hello ")
    t.render(Context("name" -> Value.of(""))) should be ("Hello ")
    t.render(Context("name" -> Value.of(false))) should be ("Hello ")
    t.render(Context("name" -> Value.list())) should be ("Hello ")
  }

  it should "render inverted sections" in {
    val t = Template(Literal("Hello "), Section("name", Seq(Literal("World!")), inverted = true))
    t.render(Context.empty) should be ("Hello World!")
    t.render(Context("name" -> Value.of(""))) should be ("Hello World!")
    t.render(Context("name" -> Value.of("haha"))) should be ("Hello ")
    t.render(Context("name" -> Value.of(false))) should be ("Hello World!")
    t.render(Context("name" -> Value.of(true))) should be ("Hello ")
    t.render(Context("name" -> Value.list())) should be ("Hello World!")
    t.render(Context("name" -> Value.list("haha"))) should be ("Hello ")
  }

  it should "render simple lists" in {
    val t = Template(Section("colors", Seq(Literal("- "), Variable("."), Literal("\n"))))
    val context = Context("colors" -> Value.list("red", "green", "yellow"))
    t.render(context) should be ("- red\n- green\n- yellow\n")
  }

  it should "render list of objects" in {
    val t = Template(Section("colors", Seq(Literal("- "), Variable("name"), Literal("\n"))))
    val context = Context("colors" -> Value.list(
      Value.map("name" -> "red"), Value.map("name" -> "green"), Value.map("name" -> "yellow")))
    t.render(context) should be ("- red\n- green\n- yellow\n")
  }

  it should "render lambda values" in {
    val t = Template(Section("colors", Seq(Literal("- "), Variable("name"), Literal("\n"))))
    val context = Context(
      "colors" -> Value.lambda(s =>
        Expand.variableExpand.asString(Variable("name"))
      ),
      "name" -> "Willy"
    )
    t.render(context) should be ("Willy")
    t.render(Context("colors" -> Value.lambda(s => ContextGet.unit(s.asString)))) should be (
      "{{#colors}}- {{name}}\n{{/colors}}"
    )
  }

  it should "print template strings" in {
    Template(Literal("Hello"), Literal(" "), Literal("world!")).asString should be (
      "Hello world!"
    )
    Template(Literal("Hello "), Variable("name"), Literal("!")).asString should be (
      "Hello {{name}}!"
    )
    Template(Literal("Hello "), Section("name", Seq(Literal("World!")), inverted = true)).asString should be (
      "Hello {{^name}}World!{{/name}}"
    )
    Template(Section("colors", Seq(Literal("- "), Variable("."), Literal("\n")))).asString should be (
      "{{#colors}}- {{.}}\n{{/colors}}"
    )
    Template(Section("colors", Seq(Literal("- "), Variable("name"), Literal("\n")))).asString should be (
      "{{#colors}}- {{name}}\n{{/colors}}"
    )
  }

  it should "thread context through" in {
    val ctx0 = new Context {
      def find(key: String): (Context, Option[Value]) = {
        (this, if (key == "x1") Some(Value.of("red")) else None)
      }
    }
    val ctx1 = new Context {
      def find(key: String): (Context, Option[Value]) = {
        (ctx0, if (key == "x2") Some(Value.of("blue")) else None)
      }
    }
    val t0 = Template(Seq(Variable("x2"), Literal("-"), Variable("x1")))
    t0.render(ctx1) should be ("blue-red")
    t0.render(ctx0) should be ("-red")

    val t1 = Template(Seq(Variable("x1"), Literal("-"), Variable("x2")))
    t1.render(ctx1) should be ("-")
    t1.render(ctx0) should be ("red-")
  }

  "parser" should "parse literal text" in {
    val Success(text0, _) = Parser.literal.parse("hello world!")
    text0 should be (Literal("hello world!"))

    val Success(text1, _) = Parser.literal.parse("hello {{name}}!")
    text1 should be (Literal("hello "))
  }

  it should "parse variables" in {
    val Success(var0, _) = Parser.variable.parse("{{name}}")
    var0 should be (Variable("name"))
    val Success(var1, _) = Parser.variable.parse("{{& name}}")
    var1 should be (Variable("name", true))
  }

  it should "parse sections" in {
    val inputs = List(
      "{{#colors}}- {{name}}\n{{/colors}}",
      "{{#colors}}- {{.}}\n{{/colors}}"
    )
    for (in <- inputs) {
      val Success(t, _) = Parser.template.parse(in)
      t.els(0) shouldBe a [Section]
      t.asString should be (in)
    }
  }
}
