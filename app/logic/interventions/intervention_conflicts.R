box::use(
  checkmate[qtest],
  config,
  logger[log_debug],
)

# fmt: skip
box::use(
  app/logic/core/logging[log_ns_builder, log_ns_fn],
)

#' Get mutual exclusivity rules for interventions
#'
#' Returns hard-coded rules defining which interventions cannot be deployed
#' together in the same district. These rules are based on domain knowledge
#' about malaria intervention programs.
#'
#' @return Named list where each element is a character vector of mutually
#'   exclusive intervention column names. Only one intervention from each group
#'   can be active per district.
#'
#' @details
#' Current exclusivity groups:
#' - **case_management**: Only one case management type (CM or ICCM)
#' - **net_types**: Only one net type (STD_Nets, PBO_Nets, or IG2_Nets)
#' - **preventive_chemo**: Only one preventive chemotherapy (PMC or SMC)
#'
#' @export
get_mutual_exclusivity_rules <- function() {
  # config returns lists of lists; convert inner lists to character vectors
  lapply(config$get("intervention_exclusivity"), unlist)
}

#' Detect conflicts in intervention selection
#'
#' Analyzes a list of selected interventions for a district to identify
#' violations of mutual exclusivity rules. Returns detailed information about
#' any conflicts found.
#'
#' @param intervention_list Character vector of intervention column names (e.g.,
#'   "deployed_int_CM", "deployed_int_STD_Nets")
#' @param district_name String name of district (used for logging and error
#'   messages)
#'
#' @return List with three components:
#' \describe{
#'   \item{has_conflict}{Logical: TRUE if any conflicts detected}
#'   \item{conflict_groups}{Named list of conflicting intervention groups. Each
#'     element contains the interventions from that group that were selected}
#'   \item{details}{Character vector of human-readable conflict descriptions}
#' }
#'
#' @examples
#' \dontrun{
#' # Detect conflict between CM and ICCM
#' detect_conflicts(
#'   c("deployed_int_CM", "deployed_int_ICCM", "deployed_int_STD_Nets"),
#'   "District_A"
#' )
#' # Returns: has_conflict = TRUE, conflict in case_management group
#'
#' # No conflict
#' detect_conflicts(
#'   c("deployed_int_CM", "deployed_int_STD_Nets", "deployed_int_IRS"),
#'   "District_B"
#' )
#' # Returns: has_conflict = FALSE
#' }
#'
#' @export
detect_conflicts <- function(intervention_list, district_name, ...) {
  stopifnot(
    "intervention_list must be a character vector" = qtest(
      intervention_list,
      "S*"
    )
  )
  stopifnot(
    "district_name must be a non-empty string" = qtest(district_name, "S1") &&
      district_name != ""
  )
  args <- list(...)
  log_ns <- log_ns_builder(args, as.character(match.call()[[1]]))

  rules <- get_mutual_exclusivity_rules()

  conflicts <- list(
    has_conflict = FALSE,
    conflict_groups = list(),
    details = character(0)
  )

  # Check each exclusivity group
  for (group_name in names(rules)) {
    group_interventions <- rules[[group_name]]
    selected_in_group <- intersect(intervention_list, group_interventions)

    # Conflict if more than one from this group is selected
    if (length(selected_in_group) > 1) {
      conflicts$has_conflict <- TRUE
      conflicts$conflict_groups[[group_name]] <- selected_in_group

      # Create human-readable intervention names (remove prefix)
      int_names <- gsub("^deployed_int_", "", selected_in_group)

      conflicts$details <- c(
        conflicts$details,
        paste0(
          "In ",
          district_name,
          ": Multiple interventions from '",
          group_name,
          "' group selected: ",
          paste(int_names, collapse = ", "),
          ". Only one allowed per district."
        )
      )
    }
  }

  if (conflicts$has_conflict) {
    log_debug(
      paste0(
        "Conflicts detected for ",
        district_name,
        ": ",
        paste(names(conflicts$conflict_groups), collapse = ", ")
      ),
      namespace = log_ns_fn(log_ns)
    )
  }

  conflicts
}

#' Resolve conflicts using priority system
#'
#' When multiple mutually exclusive interventions are selected for a district,
#' this function resolves the conflict by keeping only the highest-priority
#' intervention from each conflicting group.
#'
#' @param interventions Character vector of selected intervention column names
#' @param conflicts List returned by `detect_conflicts()`
#' @param priorities Named numeric vector where names are intervention column
#'   names and values are priority scores (higher = more important)
#'
#' @return Character vector of resolved interventions with conflicts removed
#'
#' @details
#' For each conflicting group:
#' 1. Identifies which interventions from the group were selected
#' 2. Looks up their priority values
#' 3. Keeps only the intervention with the highest priority
#' 4. Removes all lower-priority interventions from that group
#'
#' If no conflicts exist, returns the original intervention list unchanged.
#'
#' @examples
#' \dontrun{
#' interventions <- c("deployed_int_CM", "deployed_int_ICCM")
#' conflicts <- detect_conflicts(interventions, "District_A")
#' priorities <- c(deployed_int_CM = 10, deployed_int_ICCM = 9)
#'
#' resolve_conflicts(interventions, conflicts, priorities)
#' # Returns: c("deployed_int_CM")  # Kept higher priority
#' }
#'
#' @export
resolve_conflicts <- function(interventions, conflicts, priorities, ...) {
  stopifnot(
    "interventions must be a character vector" = qtest(interventions, "S*")
  )
  stopifnot("conflicts must be a list" = qtest(conflicts, "l"))
  stopifnot(
    "priorities must be a named numeric vector" = qtest(priorities, "N+") &&
      !is.null(names(priorities))
  )
  args <- list(...)
  log_ns <- log_ns_builder(args, as.character(match.call()[[1]]))

  # No conflicts, return original
  if (!conflicts$has_conflict) {
    return(interventions)
  }

  resolved <- interventions

  # Process each conflicting group
  for (group_name in names(conflicts$conflict_groups)) {
    conflicting <- conflicts$conflict_groups[[group_name]]

    # Get priorities for conflicting interventions
    priorities_subset <- priorities[conflicting]

    # Check if all priorities are available
    if (any(is.na(priorities_subset))) {
      missing <- names(priorities_subset)[is.na(priorities_subset)]
      warning(paste0(
        "Missing priorities for interventions: ",
        paste(missing, collapse = ", "),
        ". Using first intervention in group."
      ))
      keep_intervention <- conflicting[1]
    } else {
      # Keep only highest priority intervention
      keep_intervention <- names(which.max(priorities_subset))
    }

    remove_interventions <- setdiff(conflicting, keep_intervention)

    # Remove lower priority interventions
    resolved <- setdiff(resolved, remove_interventions)

    log_debug(
      paste0(
        "Resolved conflict in group '",
        group_name,
        "': ",
        "kept ",
        gsub("^deployed_int_", "", keep_intervention),
        " ",
        "(priority=",
        priorities[keep_intervention],
        "), ",
        "removed ",
        paste(gsub("^deployed_int_", "", remove_interventions), collapse = ", ")
      ),
      namespace = log_ns_fn(log_ns)
    )
  }

  resolved
}

#' Get default intervention priorities
#'
#' Returns a named numeric vector defining the priority of each intervention for
#' conflict resolution. Higher values indicate higher priority.
#'
#' @return Named numeric vector where names are intervention column names and
#'   values are priority scores
#'
#' @details
#' Priority rationale:
#' - **Case management (CM)** is highest priority as it's core to malaria
#'   treatment
#' - **Vector control** interventions (nets, IRS, LSM) are mid-high priority
#' - **Chemoprevention** (IPTSc, PMC, SMC) are mid priority
#' - **Vaccine** is lowest priority as it's newest intervention
#'
#' These priorities are used when resolving conflicts between mutually
#' exclusive interventions.
#'
#' @export
get_intervention_priorities <- function() {
  # config returns list; unlist to get named vector
  unlist(config$get("intervention_priorities"))
}
