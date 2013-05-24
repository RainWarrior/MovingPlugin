package movingplugin

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
import nsc.transform.TypingTransformers

class MovingPlugin(val global: Global) extends Plugin { self =>
  override val name = "testplugin"
  override val description = "lalala lululu"
  override val components = List[PluginComponent](
    new Component(global, "gather"),
    new Component(global, "inject"))

  class Component(
      override val global: Global,
      override val phaseName: String) extends PluginComponent with TypingTransformers { cmp =>

    import global._
    override val runsAfter = List[String]("parser")
    override val runsBefore = List[String]("namer")
    override def newPhase(prev: Phase): Phase = new MovingPhase(prev)

    def getName(tree: Tree): Option[TermName] = tree match {
      case Ident(name: TermName) => Some(name)
      case Select(_, name: TermName) => Some(name)
      case _ => None
    }

    class MovingPhase(prev: Phase) extends StdPhase(prev) {
      override def name = self.name
      var temp = ""
      val initCode = new collection.mutable.HashMap[TermName, List[Tree]]
      def apply(unit: CompilationUnit) {
        unit.body = new MovingTransformer(prev, this, unit).transform(unit.body)
      }
    }

    class MovingTransformer(prev: Phase, cur: MovingPhase, unit: CompilationUnit) extends TypingTransformer(unit) {
      val annName = newTypeName(if(phaseName == "gather") "Gather" else "Inject")
      override def transform(tree: Tree) = tree match {
        case md @ ModuleDef(mods, name, Template(name2, val2, constDef :: defs)) =>
          mods.annotations collectFirst {
            case Apply(Select(New(Ident(`annName`)), _), params) => params
            case Apply(Select(New(Select(_, `annName`)), _), params) => params
          } map { params: List[Tree] =>
            //unit.warning(tree.pos, "PHASE " + phaseName)
            if(phaseName == "gather") {
              //unit.warning(tree.pos, "GATHER")
              params match {
                case List(param) =>
                  cur.temp = name.decoded
                  getName(param) match {
                    case Some(targetName) =>
                      unit.warning(tree.pos, f"Gathering from ${name.decoded} to ${targetName.decoded}")
                      cur.initCode(targetName) = defs ::: cur.initCode.getOrElse(targetName, Nil)
                      //EmptyTree
                      ModuleDef(mods, name, Template(name2, val2, List(constDef)))
                    case None =>
                      unit.error(tree.pos, f"Illegal target ${param}")
                      super.transform(tree)
                  }
                case List() =>
                  unit.error(tree.pos, "Not enough arguments to annotation")
                  super.transform(tree)
                case _ =>
                  unit.error(tree.pos, "Too many arguments to annotation")
                  super.transform(tree)
              }
            } else {
              //unit.warning(tree.pos, "INJECT")
              prev match {
                case pr if pr.isInstanceOf[MovingPhase] => // ugly
                  val prv = pr.asInstanceOf[MovingPhase]
                  unit.warning(tree.pos,
                    f"Injecting to ${name.decoded}: "
                    + prv.initCode.getOrElse(name, Nil))
                  ModuleDef(
                    mods,
                    name,
                    Template(
                      name2,
                      val2,
                      constDef :: (prv.initCode.getOrElse(name, Nil) ::: defs)))
                  //super.transform(tree)
                case _ =>
                  unit.error(tree.pos, "Illegal compilation state: " + prev.getClass.getName)
                  super.transform(tree)
              }
            }
          } getOrElse {
            super.transform(tree)
          }
        case _ => super.transform(tree)
      }
    }
  }
}

