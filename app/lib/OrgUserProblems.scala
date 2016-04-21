/*
 * Copyright 2014 The Guardian
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

package lib

import org.kohsuke.github.{GHIssue, GHUser, GHOrganization}
import Implicits._
import collection.convert.wrapAsScala._
import play.api.Logger
import views.html._
import com.github.nscala_time.time.Imports._

case class OrgUserProblems(org: GHOrganization, user: GHUser, applicableRequirements: Set[AccountRequirement], problems: Set[AccountRequirement]) {

  lazy val applicableLabels: Set[String] = applicableRequirements.map(_.issueLabel)

  def createIssue() {
    require(problems.nonEmpty)

    if (org.testMembership(user)) {
      if (Utility.isLogOnly) {
        Logger.info(s"[LOGONLY] Would have created issue for ${user.getLogin} because of $problems")
      } else {
        Logger.info(s"Creating issue for ${user.getLogin} $problems")

        val title = s"@${user.getLogin}: ${org.displayName} asks you to fix your GitHub account!"
        val description = views.html.ghIssues.issue(user, org, problems).body
        val issue = org.peopleRepo.createIssue(title)
        for (p <- problems) { issue.label(p.issueLabel) }
        val createdIssue = issue.assignee(user).body(description).create()
        Logger.info(s"Created issue #${createdIssue.getNumber} for ${user.getLogin}")
      }
    } else {
      Logger.info(s"No need to create an issue for ${user.getLogin} - they are no longer a member of the ${org.getLogin} org")
    }
  }

  def updateIssue(issue: GHIssue) {
    val stateUpdate = stateUpdateFor(issue)
    Logger.info(s"Updating issue for ${user.getLogin} with $stateUpdate")

    stateUpdate match {
      case UserHasLeftOrg =>
        if (Utility.isLogOnly) {
          Logger.info(s"[LOGONLY] Would have written comment on existing issue for ${user.getLogin} because they left the org")
        } else {
          issue.comment(views.html.ghIssues.userHasLeftOrg(org, user).body)
        }

      case membershipTermination: MembershipTermination =>
        if (Utility.isLogOnly) {
          if (membershipTermination.problems.isEmpty) {
            Logger.info(s"[LOGONLY] Would normally have removed ${user.getLogin} due to the schedule but it appears that they are now good to go")
          } else {
            Logger.info(s"[LOGONLY] Would have removed ${user.getLogin} because action was not taken for their assigned issue")
          }
        } else {
          if (membershipTermination.problems.isEmpty) {
            Logger.info(s"Would normally have removed ${user.getLogin} due to the schedule but it appears that they are good to go")
          } else {
            issue.comment(views.html.ghIssues.membershipTermination(user, membershipTermination)(org).body)
            org.remove(user)
          }
        }
      case update: MemberUserUpdate =>
        if (Utility.isLogOnly) {
          Logger.info(s"[LOGONLY] Would have updated issue for ${user.getLogin} because action had been taken")
        } else {
          if (update.orgMembershipWillBeConcealed) {
            org.conceal(user)
          }

          if (update.worthyOfComment) {
            issue.comment(views.html.ghIssues.memberUserUpdate(update)(org).body)
          }

          val oldLabelSet = issue.labelNames.toSet
          val unassociatedLabels = oldLabelSet -- applicableLabels
          val newLabelSet = problems.map(_.issueLabel) ++ unassociatedLabels ++ update.terminationWarning.map(_.warnedLabel)

          if (newLabelSet != oldLabelSet) issue.setLabels(newLabelSet.toSeq: _*)
        }
      case default =>
        Logger.info(s"default case hit for updateIssue. Please ensure that a new explicit case is created, which fully respects the logonly configuration.")
    }

    if (stateUpdate.issueCanBeClosed) {
      if (Utility.isLogOnly) {
        Logger.info(s"[LOGONLY] Would have closed existing issue for ${user.getLogin} because all actions required are complete")
      } else {
        issue.close()
      }
    }
  }

  def stateUpdateFor(issue: GHIssue): StateUpdate = {
    if (org.testMembership(user)) {
      val oldLabels = issue.getLabels.map(_.getName).toSet

      val oldBotLabels = oldLabels.filter(applicableLabels)

      val oldProblems = oldBotLabels.map(AccountRequirements.RequirementsByLabel)

      val schedule = TerminationSchedule.Relaxed

      val terminationDate = schedule.terminationDateFor(issue)

      val now = DateTime.now

      if (now > terminationDate) MembershipTermination(problems) else {

        val userShouldBeWarned = problems.nonEmpty && now > (terminationDate - schedule.finalWarningPeriod)

        val userHasBeenWarned = issue.getLabels.exists(_.getName == schedule.warnedLabel)

        val userShouldReceiveFinalWarning = userShouldBeWarned && !userHasBeenWarned

        MemberUserUpdate(
          oldProblems,
          problems,
          terminationDate,
          orgMembershipWillBeConcealed = problems.nonEmpty && org.hasPublicMember(user),
          terminationWarning = Some(schedule).filter(_ => userShouldReceiveFinalWarning)
        )
      }

    } else UserHasLeftOrg
  }
}
