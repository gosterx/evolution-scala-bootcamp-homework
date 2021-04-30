//package com.evolution.bootcamp.homework.effects
//
//import com.evolution.bootcamp.homework.effects.EffectsHomework1.IO
//import org.scalatest.Matchers
//import org.scalatest.freespec.AnyFreeSpec
//import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
//
//class EffectsHomework1Spec extends AnyFreeSpec with Matchers with ScalaCheckPropertyChecks {
//
//  "IO" - {
//    "map works correctly" in {
//      forAll{ (x:Int, y: Int) => {
//        new IO(() => x).map(_ + y).unsafeRunSync() shouldBe x + y
//      }}
//    }
//
//    "flatMap works correctly" in {
//      forAll{ (x:Int, y: Int) => {
//        new IO(() => x).flatMap(v => new IO(() => v + y)).unsafeRunSync() shouldBe x + y
//      }}
//    }
//
//    "*> works correctly" in {
//      forAll{ (x:Int, y: Int) => {
//        new IO(() => x).*>(new IO(() => y)).unsafeRunSync() shouldBe y
//      }}
//    }
//
//    "as works correctly" in {
//      forAll{ (x:Int, y: Int) => {
//        new IO(() => x).as(y).unsafeRunSync() shouldBe y
//      }}
//    }
//
//    "void works correctly" in {
//      forAll{ (x:Int, y: Int) => {
//        new IO(() => x).void.unsafeRunSync() shouldBe ()
//      }}
//    }
//
//    "attempt works correctly with" - {
//      "right either channel" in {
//        forAll{ (x:Int) => {
//          new IO(() => x).attempt.unsafeRunSync() shouldBe Right(x)
//        }}
//      }
//      "left either channel" in{
//        forAll{ (x: Throwable) => {
//          new IO(() => throw x).attempt.unsafeRunSync() shouldBe Left(x)
//        }}
//      }
//    }
//
//    "option works correctly with" - {
//      "some channel" in {
//        forAll{ (x:Int) => {
//          new IO(() => x).option.unsafeRunSync() shouldBe Some(x)
//        }}
//      }
//      "none channel" in{
//        forAll{ (x: Throwable) => {
//          new IO(() => throw x).option.unsafeRunSync() shouldBe None
//        }}
//      }
//    }
//
//    "handleErrorWith works correctly with" - {
//      "success channel" in {
//        forAll{ (x:Int, y: Int) => {
//          new IO(() => x).handleErrorWith(_ => new IO(() => y)).unsafeRunSync() shouldBe x
//        }}
//      }
//      "failure channel" in{
//        forAll{ (x: Throwable, y: Int) => {
//          new IO(() => throw x).handleErrorWith(_ => new IO(() => y)).unsafeRunSync() shouldBe y
//        }}
//      }
//    }
//
//    "redeem works correctly with" - {
//      "success channel" in {
//        forAll{ (x:Int, y: Int, z: Int) => {
//          new IO(() => x).redeem(_ => y, _ => z).unsafeRunSync() shouldBe z
//        }}
//      }
//      "failure channel" in{
//        forAll{ (x: Int, y: Int, z: Int) => {
//          new IO(() => x / 0).redeem(_ => y, _ => z).unsafeRunSync() shouldBe y
//        }}
//      }
//    }
//
//    "redeemWith works correctly with" - {
//      "success channel" in {
//        forAll{ (x:Int, y: Int, z: Int) => {
//          new IO(() => x).redeemWith(_ => new IO(() => y), _ => new IO(() => z)).unsafeRunSync() shouldBe z
//        }}
//      }
//      "failure channel" in{
//        forAll{ (x: Int, y: Int, z: Int) => {
//          new IO(() => x / 0).redeemWith(_ => new IO(() => y), _ => new IO(() => z)).unsafeRunSync() shouldBe y
//        }}
//      }
//    }
//
//  }
//
//}
