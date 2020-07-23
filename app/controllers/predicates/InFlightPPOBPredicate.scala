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

package controllers.predicates

import common.SessionKeys.inFlightContactDetailsChangeKey
import config.AppConfig
import models.User
import play.api.Logger
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.Results.{Conflict, Redirect}
import play.api.mvc.{ActionRefiner, Result}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.HeaderCarrierConverter

import scala.concurrent.{ExecutionContext, Future}

class InFlightPPOBPredicate (inFlightComps: InFlightPPOBPredicateComponents,
                                      blockIfPendingPref: Boolean,
                                      redirectURL: String)
  extends ActionRefiner[User, User] with I18nSupport {

  implicit val appConfig: AppConfig = inFlightComps.appConfig
  implicit val executionContext: ExecutionContext = inFlightComps.mcc.executionContext
  override def messagesApi: MessagesApi = inFlightComps.mcc.messagesApi

  override def refine[A](request: User[A]): Future[Either[Result, User[A]]] = {

    implicit val hc: HeaderCarrier = HeaderCarrierConverter.fromHeadersAndSession(request.headers, Some(request.session))
    implicit val user: User[A] = request

    user.session.get(inFlightContactDetailsChangeKey) match {
      case Some("false") => Future.successful(Right(user))
      case Some("commsPref") if blockIfPendingPref => Future.successful(Left(Conflict(inFlightComps.changePendingView())))
      case Some("true") => Future.successful(Left(Conflict(inFlightComps.changePendingView())))
      case _ => getCustomerInfoCall(user.vrn)
    }
  }

  private def getCustomerInfoCall[A](vrn: String)
                                    (implicit hc: HeaderCarrier, request: User[A]): Future[Either[Result, User[A]]] =
    inFlightComps.customerCircumstancesService.getCustomerCircumstanceDetails(vrn).map {
      case Right(customerInfo) =>
        customerInfo.pendingChanges match {
          case Some(changes) if blockIfPendingPref && changes.commsPreference.isDefined =>
            Left(Conflict(inFlightComps.changePendingView()).addingToSession(inFlightContactDetailsChangeKey -> "commsPref"))
          case Some(changes) if changes.ppob.isDefined =>
            Left(Conflict(inFlightComps.changePendingView()).addingToSession(inFlightContactDetailsChangeKey -> "true"))
          case _ =>
            Logger.debug("[InFlightPredicate][getCustomerInfoCall] - There are no in-flight changes. " +
              "Redirecting user to the start of the journey.")
            Left(Redirect(redirectURL).addingToSession(inFlightContactDetailsChangeKey -> "false"))
        }
      case Left(error) =>
        Logger.warn("[InFlightPredicate][getCustomerInfoCall] - " +
          s"The call to the GetCustomerInfo API failed. Error: ${error.message}")
        Left(inFlightComps.serviceErrorHandler.showInternalServerError)
    }

}
