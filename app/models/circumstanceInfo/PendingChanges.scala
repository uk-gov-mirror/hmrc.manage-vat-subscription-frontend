/*
 * Copyright 2020 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package models.circumstanceInfo

import models.returnFrequency.ReturnPeriod
import play.api.libs.functional.syntax._
import play.api.libs.json.{Json, Reads, Writes, __}

case class PendingChanges(ppob: Option[PPOB],
                          bankDetails: Option[BankDetails],
                          returnPeriod: Option[ReturnPeriod],
                          commsPreference: Option[String])

object PendingChanges {

  private val ppobPath = __ \ "PPOBDetails"
  private val bankDetailsPath =  __ \ "bankDetails"
  private val returnPeriodPath = __ \ "returnPeriod"
  private val commsPreferencePath = __ \ "commsPreference"

  implicit val reads: Reads[PendingChanges] = (
    ppobPath.readNullable[PPOB] and
    bankDetailsPath.readNullable[BankDetails] and
    returnPeriodPath.readNullable[ReturnPeriod] and
    commsPreferencePath.readNullable[String]
  )(PendingChanges.apply _)

  implicit val writes: Writes[PendingChanges] = (
    ppobPath.writeNullable[PPOB] and
    bankDetailsPath.writeNullable[BankDetails] and
    returnPeriodPath.writeNullable[ReturnPeriod] and
    commsPreferencePath.writeNullable[String]
  )(unlift(PendingChanges.unapply))

  val auditWrites: Writes[Option[PendingChanges]] = Writes {
    case Some(pending) =>
      Json.obj(
        "businessAddress" -> pending.ppob.isDefined,
        "repaymentBankDetails" -> pending.bankDetails.isDefined,
        "vatReturnDates" -> pending.returnPeriod.isDefined,
        "emailAddress" -> pending.ppob.fold(false)(_.contactDetails.fold(false)(_.emailAddress.isDefined)),
        "commsPreference" -> pending.commsPreference.isDefined
      )
    case _ =>
      Json.obj(
        "businessAddress" -> false,
        "repaymentBankDetails" -> false,
        "vatReturnDates" -> false,
        "emailAddress" -> false,
        "commsPreference" -> false
      )
  }
}
