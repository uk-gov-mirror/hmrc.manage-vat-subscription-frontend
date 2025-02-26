/*
 * Copyright 2021 HM Revenue & Customs
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

package controllers

import assets.BaseTestConstants._
import assets.CircumstanceDetailsTestConstants.{customerInformationModelMaxOrganisationPending, customerInformationWithPartyType}
import assets.PaymentsTestConstants._
import audit.models.BankAccountHandOffAuditModel
import mocks.services.MockPaymentsService
import org.jsoup.Jsoup
import org.mockito.ArgumentMatchers
import org.mockito.Mockito.verify
import play.api.http.Status
import play.api.test.Helpers.{redirectLocation, _}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext

class PaymentsControllerSpec extends ControllerBaseSpec with MockPaymentsService {

  object TestPaymentController extends PaymentsController(
    mockAuthPredicate,
    serviceErrorHandler,
    mockPaymentsService,
    mockAuditingService,
    mockCustomerDetailsService,
    mockInFlightRepaymentBankAccountPredicate,
    mcc,
    mockConfig,
    ec
  )

  "Calling the sendToPayments method for an individual" when {

    "user has no in-flight repayment bank account change" when {

      def setup(customerDetailsResponse: CircumstanceDetailsResponse = Right(customerInformationWithPartyType(None)),
                paymentsResponse: PaymentsResponse): PaymentsController = {

        setupMockPaymentsService(paymentsResponse)
        setupMockCustomerDetails(vrn)(customerDetailsResponse)
        mockIndividualAuthorised()

        TestPaymentController
      }

      "the PaymentsService returns a Right(PaymentRedirectModel)" should {

        lazy val controller = setup(paymentsResponse = Right(successPaymentsResponseModel))
        lazy val result = controller.sendToPayments(request)

        "return 303 (Redirect)" in {
          status(result) shouldBe Status.SEE_OTHER

          verify(mockAuditingService)
            .extendedAudit(
              ArgumentMatchers.eq(BankAccountHandOffAuditModel(user, successPaymentsResponse)),
              ArgumentMatchers.eq[Option[String]](Some(controllers.routes.PaymentsController.sendToPayments().url))
            )(
              ArgumentMatchers.any[HeaderCarrier],
              ArgumentMatchers.any[ExecutionContext]
            )
        }

        "redirect to the correct url" in {
          redirectLocation(result) shouldBe Some(successPaymentsResponse)
        }
      }

      "the PaymentsService returns an error" should {

        lazy val controller = setup(paymentsResponse = Left(errorModel))
        lazy val result = controller.sendToPayments(request)

        "return 500 (ISE)" in {
          status(result) shouldBe INTERNAL_SERVER_ERROR
          messages(Jsoup.parse(bodyOf(result)).title) shouldBe internalServerErrorTitleUser
        }
      }

      "the CustomerCircumstanceDetailsService returns an error" should {

        lazy val controller = setup(customerDetailsResponse = Left(errorModel), paymentsResponse = Left(errorModel))
        lazy val result = controller.sendToPayments(request)

        "return 500 (ISE)" in {
          status(result) shouldBe INTERNAL_SERVER_ERROR
          messages(Jsoup.parse(bodyOf(result)).title) shouldBe internalServerErrorTitleUser
        }
      }
    }

    "user has an in-flight repayment bank account change" should {

      lazy val result = TestPaymentController.sendToPayments(request)

      "return SEE_OTHER (303)" in {
        mockCustomerDetailsSuccess(customerInformationModelMaxOrganisationPending)
        status(result) shouldBe Status.SEE_OTHER
      }

      s"redirect to ${controllers.routes.CustomerCircumstanceDetailsController.redirect().url}" in {
        redirectLocation(result) shouldBe Some(controllers.routes.CustomerCircumstanceDetailsController.redirect().url)
      }
    }

    insolvencyCheck(TestPaymentController.sendToPayments)
  }
}
