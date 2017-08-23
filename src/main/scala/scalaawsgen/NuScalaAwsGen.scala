package scalaawsgen

import java.io.File

import io.circe._
import io.circe.optics.JsonPath._
import io.circe.parser.parse
import monocle._
import monocle.function.Each

import scala.io.Source.fromFile
import scalaz.Applicative
import cats.implicits._
import io.circe.optics.JsonPath

object NuScalaAwsGen {

  implicit final lazy val objectEachKV: Each[JsonObject, (String, Json)] = new Each[JsonObject, (String, Json)] {
    final def each: Traversal[JsonObject, (String, Json)] = new Traversal[JsonObject, (String, Json)] {
        final def modifyF[F[_]](f: ((String, Json)) => F[(String, Json)])(from: JsonObject)(implicit
          F: Applicative[F]
        ): F[JsonObject] =
            F.map(from.toList.reverse.foldLeft(F.pure(List.empty[(String, Json)])) {
              case (acc, kv) => F.apply2(acc, f(kv)){case (list, elem) => elem :: list}
              })(JsonObject.from(_))
      }
  }


  def main(args: Array[String]): Unit = {
    val dir = new File("../ResourceSpec/")
    val files = dir.listFiles().filter(_.getName.endsWith(".json"))
    files.foreach(f => process(f.getPath))
  }

  def process(filename: String): Unit = {
    val result = for {
      json <- parse(fromFile(filename).mkString)
      propTypes <- getPropertyTypes(json)
      resTypes <- getResourceTypes(json, propTypes)
    } yield (propTypes, resTypes)

    println(s"\n------------| Processing $filename |---------------------------------------------------------->")
    println(s"PropertyTypes: ${result.map(_._1.mkString(", "))}.")
    println(s"\nResourceTypes: ${result.map(_._2.mkString(",  "))}.")
  }

  def getPropertyTypes(json: Json): Either[String, List[SubPropertyType]] = {
    val propertyTypes = getAllKVs(root.PropertyTypes, json).sortBy(_._1)
    propertyTypes.foldM(List.empty[SubPropertyType])((pts, kv) => {
      val (fullName, propTypeJson) = kv
      val name = fullName.split("\\.").last
      val props = (root.Properties.obj composeTraversal objectEachKV.each).getAll(propTypeJson)
      val tryProps: Either[String, List[ResourceProperty]] = props.traverse(kv => {
        val (propName, propJson) = kv
        for {
          typ <- getType(propJson, pts)
          required <- isRequiredProperty(propJson)
        }
        yield ResourceProperty(propName, typ, required)
      })
      tryProps.map(ps => SubPropertyType(name, ps.toSet) +: pts)
    })
  }

  def getAllKVs(path: JsonPath, json: Json): List[(String, Json)] = {
    (path.obj composeTraversal objectEachKV.each).getAll(json)
  }

  def getType(propertyJson: Json, propTypes: List[SubPropertyType]): Either[String, PropertyType] = {
    val tryPrim = (root.PrimitiveType.string).getOption(propertyJson).toRight("PrimitiveType").flatMap(PrimitiveType.fromString)
    val tryListPrim = tryPrim.recoverWith{
      case err => for {
          _ <- (root.Type.string).getOption(propertyJson).filter(_ == "List").toRight(s"$err, Type=List")
          ps <- (root.PrimitiveItemType.string).getOption(propertyJson).toRight(s"$err, Type=List, PrimitiveItemType")
          p <- PrimitiveType.fromString(ps)
        }
        yield ListPropertyType(p)
    }
    val tryListSubprop = tryListPrim.recoverWith{
      case err => for {
        n <- (root.ItemType.string).getOption(propertyJson).toRight(s"$err, ItemType")
        pt <- propTypes.find(_.name == n).toRight(s"Unknown subproperty: $n")
      } yield (pt)
    }
    val tryMapPrim = tryListSubprop.recoverWith{
      case err => for {
          _ <- (root.Type.string).getOption(propertyJson).filter(_ == "Map").toRight(s"$err, Type=Map")
          ps <- (root.PrimitiveItemType.string).getOption(propertyJson).toRight(s"$err, Type=Map, PrimitiveItemType")
          p <- PrimitiveType.fromString(ps)
        }
        yield MapPropertyType(p)
    }
    val tryPropTypes = tryMapPrim.recoverWith{
      case err => (for {
        n <- (root.Type.string).getOption(propertyJson)
        pt <- propTypes.find(_.name == n)
      } yield pt).toRight(s"$err, Type in [${propTypes.map(_.name).mkString(", ")}]")
    }

    //ForwardRefSubproperty is when the json references a name we haven't seen yet
    //we store the name and try to resolve it into a resource ref later after all the jsons are parsed
    val tryForwardRef = tryPropTypes.recoverWith{
      case err => (for {
        n <- (root.Type.string).getOption(propertyJson).filterNot(_ == "List")
      } yield ForwardRefSubproperty(n)).toRight(s"$err, Type in [${propTypes.map(_.name).mkString(", ")}]")
    }
    tryForwardRef.left.map(fields => s"Could find any of ($fields) in: $propertyJson")
  }

  def isRequiredProperty(propertyJson: Json): Either[String, Boolean] = {
    (root.Required.boolean).getOption(propertyJson).toRight(s"Expected key `Required` in $propertyJson")
  }

  def getResourceTypes(json: Json, propTypes: List[SubPropertyType]): Either[String, List[ResourceType]] = {
    val resourceTypes = getAllKVs(root.ResourceType, json)

    //for each Resource in the json document
    resourceTypes.traverse(kv => {
      val (name, resTypeJson) = kv

      //try resolve all its Properties
      val props = getAllKVs(root.Properties, resTypeJson)
      val tryProps: Either[String, List[ResourceProperty]] = props.traverse(kv => {
        val (propName, propJson) = kv
        for {
          typ <- getType(propJson, propTypes)
          required <- isRequiredProperty(propJson)
        }
        yield ResourceProperty(propName, typ, required)
      })

      //try resolve all its Attributes
      val attrsJson = getAllKVs(root.Attributes, resTypeJson)
      val tryAttrs: Either[String, List[ResourceAttribute]] = attrsJson.traverse(kv => {
        val (attrName, attrJson) = kv
        for {
          //cant tell what the real difference between a AttributeType vs a PropertyType is, if any
          //treat them as interchangable for now
          typ <- getType(attrJson, propTypes).map(_.asInstanceOf[AttributeType])
        } yield (ResourceAttribute(attrName, typ))
      })

      val Array(aws, group, resName) = name.split("::")
      for {
        ps <- tryProps
        as <- tryAttrs
      } yield (ResourceType(resName, ResourceGroup(resName), ps.toSet, as.toSet))
    })
  }


}

sealed trait PropertyType

trait AttributeType extends PropertyType

trait PrimitiveType extends AttributeType

object PrimitiveType {


  def fromString(s: String): Either[String, PrimitiveType] = s match {
    case "String" => StringPropertyType.asRight
    case "Long" => LongPropertyType.asRight
    case "Integer" => IntegerPropertyType.asRight
    case "Double" => DoublePropertyType.asRight
    case "Boolean" => BooleanPropertyType.asRight
    case "Timestamp" => TimestampPropertyType.asRight
    case "Json" => JsonPropertyType.asRight
    case _ => s"Not a PrimitiveType: $s".asLeft
  }
}

case object StringPropertyType extends PrimitiveType

case object LongPropertyType extends PrimitiveType

case object IntegerPropertyType extends PrimitiveType

case object DoublePropertyType extends PrimitiveType

case object BooleanPropertyType extends PrimitiveType

case object TimestampPropertyType extends PrimitiveType

case object JsonPropertyType extends PrimitiveType

case class ListPropertyType(itemType: PropertyType) extends AttributeType

case class MapPropertyType(itemType: PropertyType) extends PropertyType

case class SubPropertyType(name: String, properties: Set[ResourceProperty]) extends PropertyType

//Reference to a Subproperty we havent seen come up yet
case class ForwardRefSubproperty(name: String) extends PropertyType


case class ResourceGroup(name: String)

case class ResourceType(name: String, group: ResourceGroup, properties: Set[ResourceProperty], attributes: Set[ResourceAttribute])

case class ResourceProperty(name: String, typ: PropertyType, required: Boolean)

case class ResourceAttribute (name: String, typ: AttributeType)

