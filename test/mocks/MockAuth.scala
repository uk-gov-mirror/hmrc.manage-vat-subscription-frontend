/*
 * Copyright 2018 HM Revenue & Customs
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

package mocks

import controllers.predicates._
import org.mockito.ArgumentMatchers
import org.mockito.Mockito.{reset, when}
import org.mockito.stubbing.OngoingStubbing
import org.scalatest.BeforeAndAfterEach
import org.scalatest.mockito.MockitoSugar
import _root_.services.EnrolmentsAuthService
import config.ServiceErrorHandler
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.retrieve.{Retrieval, ~}
import utils.TestUtil

import scala.concurrent.Future

trait MockAuth extends TestUtil with BeforeAndAfterEach with MockitoSugar  {

  override def beforeEach(): Unit = {
    super.beforeEach()
    reset(mockAuthConnector)
    mockIndividualAuthorised()
  }

  lazy val mockAuthConnector: AuthConnector = mock[AuthConnector]

  def setupAuthResponse(authResult: Future[~[Option[AffinityGroup], Enrolments]]): OngoingStubbing[Future[~[Option[AffinityGroup], Enrolments]]] = {
    when(mockAuthConnector.authorise(
      ArgumentMatchers.any(), ArgumentMatchers.any[Retrieval[~[Option[AffinityGroup], Enrolments]]]())(
      ArgumentMatchers.any(), ArgumentMatchers.any())
    ).thenReturn(authResult)
  }

  val mockEnrolmentsAuthService: EnrolmentsAuthService = new EnrolmentsAuthService(mockAuthConnector)

  val mockAuthAsAgentWithClient: AuthoriseAsAgentWithClient = new AuthoriseAsAgentWithClient(mockEnrolmentsAuthService, messagesApi, mockAppConfig)

  val mockAuthPredicate: AuthPredicate =
    new AuthPredicate(
      mockEnrolmentsAuthService,
      messagesApi,
      mockAuthAsAgentWithClient,
      injector.instanceOf[ServiceErrorHandler],
      mockAppConfig
    )

  val mockAgentOnlyAuthPredicate: AuthoriseAsAgentOnly =
    new AuthoriseAsAgentOnly(
      mockEnrolmentsAuthService,
      messagesApi,
      injector.instanceOf[ServiceErrorHandler],
      mockAppConfig
    )

  def mockIndividualAuthorised(): OngoingStubbing[Future[~[Option[AffinityGroup], Enrolments]]] =
    setupAuthResponse(Future.successful(
      new ~(Some(AffinityGroup.Individual),
        Enrolments(Set(Enrolment("HMRC-MTD-VAT",
          Seq(EnrolmentIdentifier("VRN", "999999999")),
          "Activated"
        )))
      )
    ))

  def mockAgentAuthorised(): OngoingStubbing[Future[~[Option[AffinityGroup], Enrolments]]] =
    setupAuthResponse(Future.successful(
      new ~(Some(AffinityGroup.Agent),
        Enrolments(Set(Enrolment("HMRC-AS-AGENT",
          Seq(EnrolmentIdentifier("AgentReferenceNumber", "123456789")),
          "Activated",
          Some("mtd-vat-auth")
        )))
      )
    ))

  def mockAgentWithoutEnrolment(): OngoingStubbing[Future[~[Option[AffinityGroup], Enrolments]]] =
    setupAuthResponse(Future.successful(
      new ~(Some(AffinityGroup.Agent),
        Enrolments(Set(Enrolment("OTHER_ENROLMENT",
          Seq(EnrolmentIdentifier("", "")),
          "Activated"
        )))
      )
    ))

  def mockIndividualWithoutEnrolment(): OngoingStubbing[Future[~[Option[AffinityGroup], Enrolments]]] =
    setupAuthResponse(Future.successful(
      new ~(Some(AffinityGroup.Individual),
        Enrolments(Set(Enrolment("OTHER_ENROLMENT",
          Seq(EnrolmentIdentifier("", "")),
          ""
        )))
      )
    ))

  def mockUserWithoutAffinity(): OngoingStubbing[Future[~[Option[AffinityGroup], Enrolments]]] =
    setupAuthResponse(Future.successful(
      new ~(None,
        Enrolments(Set(Enrolment("HMRC-MTD-VAT",
          Seq(EnrolmentIdentifier("VRN", "999999999")),
          "Activated"
        )))
      )
    ))

  def mockAgentWithoutAffinity(): OngoingStubbing[Future[~[Option[AffinityGroup], Enrolments]]] =
    setupAuthResponse(Future.successful(
      new ~(None,
        Enrolments(Set(Enrolment("HMRC-AS-AGENT",
          Seq(EnrolmentIdentifier("AgentReferenceNumber", "123456789")),
          "Activated",
          Some("mtd-vat-auth")
        )))
      )
    ))

  lazy val mockMissingBearerToken: OngoingStubbing[Future[~[Option[AffinityGroup], Enrolments]]] =
    setupAuthResponse(Future.failed(MissingBearerToken()))

  lazy val mockUnauthorised: OngoingStubbing[Future[~[Option[AffinityGroup], Enrolments]]] =
    setupAuthResponse(Future.failed(InsufficientEnrolments()))

}
