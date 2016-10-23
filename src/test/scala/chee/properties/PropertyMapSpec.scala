package chee.properties

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import Generators._

object PropertyMapSpec extends Properties("PropertyMap") {

  property("++") = forAll { (map: PropertyMap) => (map ++ map) == map }

  property("no doublicate names") = forAll { (map: PropertyMap) =>
    val names = map.asSet.toList.map(_.ident)
    names.distinct == names
  }

  property("++ replaces elements in first map") = forAll {(map1: PropertyMap, map2: PropertyMap) =>
    val map3 = map1 ++ map2
    // elements are replaced, so if it's not in map2 then at must be in map1
    map3.asSet.forall(p => map2(p.ident).orElse(map1(p.ident)) == Some(p))
  }

  property("add") = forAll { (map: PropertyMap) =>
    val added = map.asSet.foldLeft(map){ (a, b) => a + b }
    added == map
  }

  property("find") = forAll { (map: PropertyMap) =>
    val names = map.asSet.map(_.ident)
    names.forall(n => map(n) != None) 
  }
}
