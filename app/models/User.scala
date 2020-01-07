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

package models

import play.api.mvc.{Request, WrappedRequest}
import uk.gov.hmrc.auth.core.{Enrolment, EnrolmentIdentifier, Enrolments, InternalError}

case class User[A](vrn: String, active: Boolean = true, arn: Option[String] = None) (implicit request: Request[A]) extends WrappedRequest[A](request) {
  val isAgent: Boolean = arn.isDefined
  val redirectSuffix: String = if(isAgent) "agent" else "non-agent"
}

object User {
  def apply[A](enrolments: Enrolments)(implicit request: Request[A]): User[A] =
    enrolments.enrolments.collectFirst {
      case Enrolment("HMRC-MTD-VAT", EnrolmentIdentifier(_, vatId) :: _, _, _) => User(vatId)
    }.getOrElse(throw InternalError("VRN Missing"))
}
