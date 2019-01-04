/*
 * Copyright 2019 HM Revenue & Customs
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

package stubs

import com.github.tomakehurst.wiremock.stubbing.StubMapping
import helpers.IntegrationTestConstants.VRN
import helpers.WireMockMethods
import play.api.http.Status.{OK, UNAUTHORIZED}
import play.api.libs.json.{JsObject, Json}
import uk.gov.hmrc.auth.core.AffinityGroup

object AuthStub extends WireMockMethods {

  private val authoriseUri = "/auth/authorise"

  private val SERVICE_ENROLMENT_KEY = "HMRC-MTD-VAT"
  private val AGENT_ENROLMENT_KEY = "HMRC-AS-AGENT"

  def authorised(): StubMapping = {
    when(method = POST, uri = authoriseUri)
      .thenReturn(status = OK, body = successfulAuthResponse(Some(AffinityGroup.Individual), mtdVatEnrolment))
  }

  def unauthorisedOtherEnrolment(): StubMapping = {
    when(method = POST, uri = authoriseUri)
      .thenReturn(status = OK, body = successfulAuthResponse(Some(AffinityGroup.Individual), otherEnrolment))
  }

  def authorisedNoAffinityGroup(): StubMapping = {
    when(method = POST, uri = authoriseUri)
      .thenReturn(status = OK, body = successfulAuthResponse(None, otherEnrolment))
  }

  def unauthorisedNotLoggedIn(): StubMapping = {
    when(method = POST, uri = authoriseUri)
      .thenReturn(status = UNAUTHORIZED, headers = Map("WWW-Authenticate" -> """MDTP detail="MissingBearerToken""""))
  }

  def agentAuthorised(): StubMapping = {
    when(method = POST, uri = authoriseUri)
      .thenReturn(status = OK, body = successfulAuthResponse(Some(AffinityGroup.Agent), agentEnrolment))
  }

  def agentUnauthorisedOtherEnrolment(): StubMapping = {
    when(method = POST, uri = authoriseUri)
      .thenReturn(status = OK, body = successfulAuthResponse(Some(AffinityGroup.Agent), otherEnrolment))
  }

  def insufficientEnrolments(): StubMapping = {
    when(method = POST, uri = authoriseUri)
      .thenReturn(status = UNAUTHORIZED, headers = Map("WWW-Authenticate" -> """MDTP detail="InsufficientEnrolments""""))
  }

  private val mtdVatEnrolment = Json.obj(
    "key" -> SERVICE_ENROLMENT_KEY,
    "identifiers" -> Json.arr(
      Json.obj(
        "key" -> "VRN",
        "value" -> VRN
      )
    )
  )

  private val agentEnrolment = Json.obj(
    "key" -> AGENT_ENROLMENT_KEY,
    "identifiers" -> Json.arr(
      Json.obj(
        "key" -> "AgentReferenceNumber",
        "value" -> "1234567890"
      )
    )
  )

  private val otherEnrolment: JsObject = Json.obj(
    "key" -> "HMRC-XXX-XXX",
    "identifiers" -> Json.arr(
      Json.obj(
        "key" -> "XXX",
        "value" -> "XXX"
      )
    )
  )

  private def successfulAuthResponse(affinityGroup: Option[AffinityGroup], enrolments: JsObject*): JsObject = {
    affinityGroup match {
      case Some(group) => Json.obj(
        "affinityGroup" -> affinityGroup,
        "allEnrolments" -> enrolments
      )
      case _ => Json.obj(
        "allEnrolments" -> enrolments
      )
    }
  }
}
