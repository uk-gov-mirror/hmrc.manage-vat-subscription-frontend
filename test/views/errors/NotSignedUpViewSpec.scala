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

package views.errors

import assets.messages.{BaseMessages, NotSignedUpPageMessages => Messages}
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import views.ViewBaseSpec
import views.html.errors.NotSignedUpView

class NotSignedUpViewSpec extends ViewBaseSpec with BaseMessages {

  val injectedView: NotSignedUpView = inject[NotSignedUpView]

  "Rendering the Not Signed Up page" should {

    lazy val view = injectedView()(request, messages, mockConfig)
    lazy implicit val document: Document = Jsoup.parse(view.body)

    "have the correct document title" in {
      document.title shouldBe Messages.title
    }

    "have a the correct page heading" in {
      elementText("#content h1") shouldBe Messages.pageHeading
    }

    "have the correct p1 on the page" in {
      paragraph(1) shouldBe Messages.text
    }

    "have the correct p2 on the page" in {
      element("#content p > a").attr("href") shouldBe mockConfig.govUkSoftwareGuidanceUrl
    }

    "A Sign Out button" which {

      "has the correct text" in {
        elementText(".govuk-button") shouldBe signOut
      }

      "has a URL to the sign out controller" in {
        element(".govuk-button").attr("href") shouldBe controllers.routes.SignOutController.signOut(false).url
      }
    }
  }
}
