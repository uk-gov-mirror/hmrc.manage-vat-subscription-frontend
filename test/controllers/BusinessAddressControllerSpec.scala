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
import assets.CircumstanceDetailsTestConstants._
import assets.CustomerAddressTestConstants._
import assets.messages.{ChangeAddressConfirmationPageMessages, ChangeAddressPageMessages, ChangePendingMessages}
import audit.models.ContactPreferenceAuditModel
import mocks.services.{MockAddressLookupService, MockBusinessAddressService, MockContactPreferenceService}
import models.contactPreferences.ContactPreference
import models.core.SubscriptionUpdateResponseModel
import models.customerAddress.AddressLookupOnRampModel
import org.jsoup.Jsoup
import org.mockito.ArgumentMatchers
import org.mockito.Mockito.{reset, verify}
import play.api.http.Status
import play.api.mvc.Result
import play.api.test.Helpers.{redirectLocation, _}
import uk.gov.hmrc.http.HeaderCarrier
import views.html.businessAddress.{ChangeAddressConfirmationView, ChangeAddressView}
import ContactPreference._
import scala.concurrent.{ExecutionContext, Future}

class BusinessAddressControllerSpec extends ControllerBaseSpec with MockAddressLookupService with
  MockBusinessAddressService with MockContactPreferenceService {

  override def afterEach(): Unit = reset(mockAuditingService)

  "Calling the .show action" when {

    object TestBusinessAddressController extends BusinessAddressController(
      mockAuthPredicate,
      mockInFlightPPOBPredicate,
      mockAddressLookupService,
      mockContactPreferenceService,
      mockBusinessAddressService,
      mockCustomerDetailsService,
      inject[ChangeAddressView],
      inject[ChangeAddressConfirmationView],
      serviceErrorHandler,
      mockAuditingService,
      mcc,
      mockConfig,
      ec
    )

    "the user is authorised and does not have any conflicting inflight data" should {

      lazy val result: Future[Result] = {
        mockCustomerDetailsSuccess(customerInformationNoPendingIndividual)
        TestBusinessAddressController.show(request)
      }

      "return OK (200)" in {
        status(result) shouldBe Status.OK
      }

      "return HTML" in {
        contentType(result) shouldBe Some("text/html")
        charset(result) shouldBe Some("utf-8")
      }

      s"have the heading '${ChangeAddressPageMessages.heading}'" in {
        messages(Jsoup.parse(bodyOf(result)).select("h1").text) shouldBe ChangeAddressPageMessages.heading
      }
    }

    "the user is authorised and has a pending change to their email address" should {

      lazy val result: Future[Result] = {
        mockCustomerDetailsSuccess(customerInformationPendingEmailModel)
        TestBusinessAddressController.show(request)
      }

      "return 409 Conflict" in {
        status(result) shouldBe Status.CONFLICT
      }

      "return HTML" in {
        contentType(result) shouldBe Some("text/html")
        charset(result) shouldBe Some("utf-8")
      }

      s"have the heading '${ChangePendingMessages.heading}'" in {
        messages(Jsoup.parse(bodyOf(result)).select("h1").text) shouldBe ChangePendingMessages.heading
      }
    }

    insolvencyCheck(TestBusinessAddressController.show)
  }

  "Calling .callback" when {

    def setup(addressLookupResponse: RetrieveAddressResponse,
              businessAddressResponse: BusinessAddressResponse): BusinessAddressController = {

      setupMockRetrieveAddress(addressLookupResponse)
      setupMockBusinessAddress(businessAddressResponse)

      new BusinessAddressController(
        mockAuthPredicate,
        mockInFlightPPOBPredicate,
        mockAddressLookupService,
        mockContactPreferenceService,
        mockBusinessAddressService,
        mockCustomerDetailsService,
        inject[ChangeAddressView],
        inject[ChangeAddressConfirmationView],
        serviceErrorHandler,
        mockAuditingService,
        mcc,
        mockConfig,
        ec)
    }

    "address lookup service returns success" when {

      "and business address service returns success" should {

        def controller: BusinessAddressController = setup(
          addressLookupResponse = Right(customerAddressMax),
          businessAddressResponse = Right(SubscriptionUpdateResponseModel(""))
        )

        "for an Individual" should {

          lazy val result = controller.callback("12345")(request)

          "return See Other (303)" in {
            status(result) shouldBe Status.SEE_OTHER
          }

          "Redirect to the confirmation page" in {
            redirectLocation(result) shouldBe Some(controllers.routes.BusinessAddressController.confirmation("non-agent").url)
          }
        }

        "for an Agent" should {

          lazy val result = controller.callback("12345")(fakeRequestWithClientsVRN)

          "return See Other (303)" in {
            mockAgentAuthorised()
            status(result) shouldBe Status.SEE_OTHER
          }

          "Redirect to the confirmation page" in {
            redirectLocation(result) shouldBe Some(controllers.routes.BusinessAddressController.confirmation("agent").url)
          }
        }
      }
    }

    "address lookup service returns success" when {

      "and business address service returns an error" should {

        lazy val controller = setup(
          addressLookupResponse = Right(customerAddressMax),
          businessAddressResponse = Left(errorModel))
        lazy val result = controller.callback("12345")(request)

        "return InternalServerError" in {
          status(result) shouldBe Status.INTERNAL_SERVER_ERROR
          messages(Jsoup.parse(bodyOf(result)).title) shouldBe internalServerErrorTitleUser
        }
      }
    }

    "address lookup service returns an error" should {

      lazy val controller = setup(
        addressLookupResponse = Left(errorModel),
        businessAddressResponse = Left(errorModel))
      lazy val result = controller.callback("12345")(request)

      "return InternalServerError" in {
        status(result) shouldBe Status.INTERNAL_SERVER_ERROR
        messages(Jsoup.parse(bodyOf(result)).title) shouldBe internalServerErrorTitleUser
      }
    }

    lazy val controller = setup(
      addressLookupResponse = Left(errorModel),
      businessAddressResponse = Left(errorModel))

    unauthenticatedCheck(controller.callback("12345"))

    insolvencyCheck(controller.callback("12345"))
  }

  "Calling .initialiseJourney" when {

    def setup(addressLookupResponse: InitialiseJourneyResponse): BusinessAddressController = {

      setupMockInitialiseJourney(addressLookupResponse)

      new BusinessAddressController(
        mockAuthPredicate,
        mockInFlightPPOBPredicate,
        mockAddressLookupService,
        mockContactPreferenceService,
        mockBusinessAddressService,
        mockCustomerDetailsService,
        inject[ChangeAddressView],
        inject[ChangeAddressConfirmationView],
        serviceErrorHandler,
        mockAuditingService,
        mcc,
        mockConfig,
        ec)
    }

    "address lookup service returns success" when {

      lazy val controller = setup(addressLookupResponse = Right(AddressLookupOnRampModel("redirect-url")))

      "the user does not have any conflicting inflight data" should {

        lazy val result = {
          mockCustomerDetailsSuccess(customerInformationNoPendingIndividual)
          controller.initialiseJourney(request)
        }

        "return redirect to the url returned" in {
          status(result) shouldBe Status.SEE_OTHER
        }

        "redirect to url returned" in {
          redirectLocation(result) shouldBe Some("redirect-url")
        }
      }

      "the user has a pending change to their email address" should {

        lazy val result = {
          mockCustomerDetailsSuccess(customerInformationPendingEmailModel)
          controller.initialiseJourney(request)
        }

        "return 409 Conflict" in {
          status(result) shouldBe Status.CONFLICT
        }

        "return HTML" in {
          contentType(result) shouldBe Some("text/html")
          charset(result) shouldBe Some("utf-8")
        }

        s"have the heading '${ChangePendingMessages.heading}'" in {
          messages(Jsoup.parse(bodyOf(result)).select("h1").text) shouldBe ChangePendingMessages.heading
        }
      }
    }

    "address lookup service returns an error" should {

      lazy val controller = setup(addressLookupResponse = Left(errorModel))
      lazy val result = {
        mockCustomerDetailsSuccess(customerInformationNoPendingIndividual)
        controller.initialiseJourney(request)
      }

      "return InternalServerError" in {
        status(result) shouldBe Status.INTERNAL_SERVER_ERROR
        messages(Jsoup.parse(bodyOf(result)).title) shouldBe internalServerErrorTitleUser
      }
    }

    insolvencyCheck(setup(addressLookupResponse = Left(errorModel)).initialiseJourney)
  }

  "calling .confirmation" when {

    lazy val controller = new BusinessAddressController(
      mockAuthPredicate,
      mockInFlightPPOBPredicate,
      mockAddressLookupService,
      mockContactPreferenceService,
      mockBusinessAddressService,
      mockCustomerDetailsService,
      inject[ChangeAddressView],
      inject[ChangeAddressConfirmationView],
      serviceErrorHandler,
      mockAuditingService,
      mcc,
      mockConfig,
      ec)

    "the user is an agent" when {

      "the call to the customer details service is successful" should {

        lazy val result = {
          mockAgentAuthorised()
          mockCustomerDetailsSuccess(customerInformationModelMaxOrganisation)
          mockContactPreferenceSuccess(ContactPreference("DIGITAL"))
          controller.confirmation("agent")(agentUser)
        }

        "return 200" in {
          status(result) shouldBe Status.OK
        }

        "return HTML" in {
          contentType(result) shouldBe Some("text/html")
          charset(result) shouldBe Some("utf-8")
        }

        "render the Business Address confirmation view" in {
          messages(Jsoup.parse(bodyOf(result)).select("h1").text) shouldBe ChangeAddressConfirmationPageMessages.heading
        }
      }

      "the call to the customer details service is unsuccessful" should {

        lazy val result = {
          mockAgentAuthorised()
          mockCustomerDetailsError()
          mockContactPreferenceSuccess(ContactPreference("DIGITAL"))
          controller.confirmation("agent")(agentUser)
        }

        "return 200" in {
          status(result) shouldBe Status.OK
        }

        "return HTML" in {
          contentType(result) shouldBe Some("text/html")
          charset(result) shouldBe Some("utf-8")
        }

        "render the Business Address confirmation view" in {
          messages(Jsoup.parse(bodyOf(result)).select("h1").text) shouldBe ChangeAddressConfirmationPageMessages.heading
        }
      }
    }

    "the user is not an agent" when {

      "contactPreference is set to 'DIGITAL'" when {

        "emailVerified feature is enabled" when {

          "the user has a verified email address" when {

            "the contactPrefMigration feature is enabled" when {

              "the call to customerCircumstanceDetails succeeds" should {
                lazy val result = {
                  mockConfig.features.contactPrefMigrationFeature(true)
                  mockConfig.features.emailVerifiedFeature(true)
                  mockConfig.features.contactPrefMigrationFeature(true)
                  mockCustomerDetailsSuccess(customerInformationModelMaxOrganisation)
                  controller.confirmation("non-agent")(request)
                }
                lazy val document = Jsoup.parse(bodyOf(result))

                "return 200" in {
                  status(result) shouldBe Status.OK

                  verify(mockAuditingService)
                    .extendedAudit(
                      ArgumentMatchers.any[ContactPreferenceAuditModel],
                      ArgumentMatchers.any[Option[String]]

                    )(
                      ArgumentMatchers.any[HeaderCarrier],
                      ArgumentMatchers.any[ExecutionContext]
                    )
                }

                "return HTML" in {
                  contentType(result) shouldBe Some("text/html")
                  charset(result) shouldBe Some("utf-8")
                }

                "render the Business Address confirmation view" in {
                  messages(document.select("h1").text) shouldBe ChangeAddressConfirmationPageMessages.heading
                  messages(document.select("#content p:nth-of-type(1)").text()) shouldBe ChangeAddressConfirmationPageMessages.digiPrefEmailVerified
                }
              }

              "the call to customerCircumstanceDetails fails" should {
                lazy val result = {
                  mockConfig.features.contactPrefMigrationFeature(true)
                  mockConfig.features.emailVerifiedFeature(true)
                  mockConfig.features.contactPrefMigrationFeature(true)
                  mockCustomerDetailsError()
                  controller.confirmation("non-agent")(request)
                }
                lazy val document = Jsoup.parse(bodyOf(result))

                "return 200" in {
                  status(result) shouldBe Status.OK
                }

                "return HTML" in {
                  contentType(result) shouldBe Some("text/html")
                  charset(result) shouldBe Some("utf-8")
                }

                "render the Business Address confirmation view" in {
                  messages(document.select("h1").text) shouldBe ChangeAddressConfirmationPageMessages.heading
                  messages(document.select("#content p:nth-of-type(1)").text()) shouldBe ChangeAddressConfirmationPageMessages.contactPrefError
                }
              }
            }

            "the user does not have a verified email address" should {
              lazy val result = {
                mockConfig.features.contactPrefMigrationFeature(true)
                mockConfig.features.emailVerifiedFeature(true)
                mockConfig.features.contactPrefMigrationFeature(true)
                mockCustomerDetailsSuccess(customerInformationModelMin.copy(commsPreference = Some(ContactPreference(digital))))
                controller.confirmation("non-agent")(request)
              }
              lazy val document = Jsoup.parse(bodyOf(result))

              "return 200" in {
                status(result) shouldBe Status.OK

                verify(mockAuditingService)
                  .extendedAudit(
                    ArgumentMatchers.any[ContactPreferenceAuditModel],
                    ArgumentMatchers.any[Option[String]]

                  )(
                    ArgumentMatchers.any[HeaderCarrier],
                    ArgumentMatchers.any[ExecutionContext]
                  )
              }

              "return HTML" in {
                contentType(result) shouldBe Some("text/html")
                charset(result) shouldBe Some("utf-8")
              }

              "render the Business Address confirmation view" in {
                messages(document.select("h1").text) shouldBe ChangeAddressConfirmationPageMessages.heading
                messages(document.select("#content p:nth-of-type(1)").text()) shouldBe ChangeAddressConfirmationPageMessages.digitalPref
              }
            }
          }

          "emailVerified feature is disabled" should {

            lazy val result = {
              mockConfig.features.contactPrefMigrationFeature(false)
              mockConfig.features.emailVerifiedFeature(false)
              mockContactPreferenceSuccess(ContactPreference("DIGITAL"))
              controller.confirmation("non-agent")(request)
            }
            lazy val document = Jsoup.parse(bodyOf(result))

            "return 200" in {
              status(result) shouldBe Status.OK

              verify(mockAuditingService)
                .extendedAudit(
                  ArgumentMatchers.any[ContactPreferenceAuditModel],
                  ArgumentMatchers.any[Option[String]]

                )(
                  ArgumentMatchers.any[HeaderCarrier],
                  ArgumentMatchers.any[ExecutionContext]
                )
            }

            "return HTML" in {
              contentType(result) shouldBe Some("text/html")
              charset(result) shouldBe Some("utf-8")
            }

            "render the Business Address confirmation view" in {
              messages(document.select("h1").text) shouldBe ChangeAddressConfirmationPageMessages.heading
              messages(document.select("#content p:nth-of-type(1)").text()) shouldBe ChangeAddressConfirmationPageMessages.digitalPref
            }
          }
        }


      }

      "contactPreference is set to 'PAPER'" should {

        lazy val result = {
          mockConfig.features.contactPrefMigrationFeature(true)
          mockCustomerDetailsSuccess(customerInformationModelMaxOrganisation.copy(commsPreference = Some(ContactPreference(paper))))
          controller.confirmation("non-agent")(request)
        }
        lazy val document = Jsoup.parse(bodyOf(result))

        "return 200" in {
          status(result) shouldBe Status.OK
        }

        "return HTML" in {
          contentType(result) shouldBe Some("text/html")
          charset(result) shouldBe Some("utf-8")
        }

        "render the Business Address confirmation view" in {
          messages(document.select("h1").text) shouldBe ChangeAddressConfirmationPageMessages.heading
          messages(document.select("#content p:nth-of-type(1)").text()) shouldBe ChangeAddressConfirmationPageMessages.paperPref
        }
      }

      "contactPreference returns an error" should {

        lazy val result = {
          mockConfig.features.contactPrefMigrationFeature(true)
          mockConfig.features.contactPrefMigrationFeature(true)
          mockCustomerDetailsError()

          controller.confirmation("non-agent")(request)
        }
        lazy val document = Jsoup.parse(bodyOf(result))

        "return 200" in {
          status(result) shouldBe Status.OK
        }

        "return HTML" in {
          contentType(result) shouldBe Some("text/html")
          charset(result) shouldBe Some("utf-8")
        }

        "render the Business Address confirmation view" in {
          messages(document.select("h1").text) shouldBe ChangeAddressConfirmationPageMessages.heading
          messages(document.select("#content p:nth-of-type(1)").text()) shouldBe ChangeAddressConfirmationPageMessages.contactPrefError
        }
      }

      insolvencyCheck(controller.confirmation("non-agent"))
    }
  }
}
