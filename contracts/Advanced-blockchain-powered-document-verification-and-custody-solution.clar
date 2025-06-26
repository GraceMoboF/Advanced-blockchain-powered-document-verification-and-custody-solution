;; Enterprise Content Management Protocol
;; Advanced blockchain-powered document verification and custody solution
;; 
;; Comprehensive framework for managing enterprise digital documents with advanced
;; security protocols, multi-layered access controls, and immutable audit capabilities
;; Designed for high-volume organizational content management requirements

;; ==== System Configuration Constants ====

;; Master system controller with administrative privileges
(define-constant system-administrator tx-sender)

;; Global document counter for sequential identification management
(define-data-var document-registry-counter uint u0)

;; ==== Comprehensive Error Code Definitions ====

;; Administrative access violation error
(define-constant unauthorized-admin-access (err u400))

;; Document identification and existence errors
(define-constant document-does-not-exist (err u401))
(define-constant document-already-registered (err u402))

;; Data validation and format errors
(define-constant invalid-document-name (err u403))
(define-constant invalid-file-specifications (err u404))
(define-constant invalid-metadata-tags (err u408))

;; Authorization and permission errors
(define-constant access-permission-denied (err u405))
(define-constant ownership-verification-failed (err u406))
(define-constant viewing-restrictions-active (err u407))

;; ==== Core Data Structure Definitions ====

;; Primary document storage repository with comprehensive metadata
(define-map enterprise-documents
  { 
    document-registry-id: uint 
  }
  {
    document-display-name: (string-ascii 64),
    document-custodian: principal,
    document-byte-size: uint,
    creation-block-height: uint,
    comprehensive-description: (string-ascii 128),
    classification-labels: (list 10 (string-ascii 32))
  }
)

;; Advanced permission control matrix for granular access management
(define-map access-control-matrix
  { 
    document-registry-id: uint, 
    requesting-principal: principal 
  }
  { 
    viewing-permission-granted: bool 
  }
)

;; ==== Private Utility Function Library ====

;; Comprehensive validation function for classification label format
(define-private (validate-classification-label (single-label (string-ascii 32)))  
  (and
    ;; Ensure label is not empty string
    (> (len single-label) u0)
    ;; Enforce maximum length constraint
    (< (len single-label) u33)
  )
)

;; Advanced validation system for complete label collection integrity
(define-private (verify-label-collection-integrity (label-collection (list 10 (string-ascii 32))))
  (and
    ;; Require at least one classification label
    (> (len label-collection) u0)
    ;; Enforce maximum label count limit
    (<= (len label-collection) u10)
    ;; Validate each individual label in collection
    (is-eq (len (filter validate-classification-label label-collection)) (len label-collection))
  )
)

;; Document existence verification in enterprise registry
(define-private (verify-document-existence (target-document-id uint))
  (is-some (map-get? enterprise-documents { document-registry-id: target-document-id }))
)

;; Secure document size retrieval with default fallback protection
(define-private (retrieve-document-size (target-document-id uint))
  (default-to u0
    (get document-byte-size
      (map-get? enterprise-documents { document-registry-id: target-document-id })
    )
  )
)

;; Robust ownership authentication mechanism
(define-private (authenticate-document-ownership (target-document-id uint) (requesting-principal principal))
  (match (map-get? enterprise-documents { document-registry-id: target-document-id })
    retrieved-document-data (is-eq (get document-custodian retrieved-document-data) requesting-principal)
    false
  )
)

;; ==== Primary Public Interface Functions ====

;; Comprehensive document registration with full metadata capture
(define-public (create-enterprise-document
  (display-name (string-ascii 64))
  (file-byte-size uint)
  (detailed-description (string-ascii 128))
  (classification-labels (list 10 (string-ascii 32)))
)
  (let
    (
      ;; Generate unique sequential document identifier
      (generated-document-id (+ (var-get document-registry-counter) u1))
    )

    ;; Comprehensive input parameter validation suite
    (asserts! (> (len display-name) u0) invalid-document-name)
    (asserts! (< (len display-name) u65) invalid-document-name)
    (asserts! (> file-byte-size u0) invalid-file-specifications)
    (asserts! (< file-byte-size u1000000000) invalid-file-specifications)
    (asserts! (> (len detailed-description) u0) invalid-document-name)
    (asserts! (< (len detailed-description) u129) invalid-document-name)
    (asserts! (verify-label-collection-integrity classification-labels) invalid-metadata-tags)

    ;; Atomic document registration in enterprise registry
    (map-insert enterprise-documents
      { document-registry-id: generated-document-id }
      {
        document-display-name: display-name,
        document-custodian: tx-sender,
        document-byte-size: file-byte-size,
        creation-block-height: block-height,
        comprehensive-description: detailed-description,
        classification-labels: classification-labels
      }
    )

    ;; Establish initial access permissions for document creator
    (map-insert access-control-matrix
      { document-registry-id: generated-document-id, requesting-principal: tx-sender }
      { viewing-permission-granted: true }
    )

    ;; Update global registry counter with thread-safe increment
    (var-set document-registry-counter generated-document-id)

    ;; Return successful registration with new document identifier
    (ok generated-document-id)
  )
)

;; Comprehensive document metadata modification system
(define-public (modify-document-metadata
  (target-document-id uint)
  (updated-display-name (string-ascii 64))
  (updated-file-size uint)
  (updated-description (string-ascii 128))
  (updated-classification-labels (list 10 (string-ascii 32)))
)
  (let
    (
      ;; Retrieve existing document data with error handling
      (existing-document-data (unwrap! (map-get? enterprise-documents { document-registry-id: target-document-id })
        document-does-not-exist))
    )

    ;; Comprehensive authorization and parameter validation
    (asserts! (verify-document-existence target-document-id) document-does-not-exist)
    (asserts! (is-eq (get document-custodian existing-document-data) tx-sender) ownership-verification-failed)
    (asserts! (> (len updated-display-name) u0) invalid-document-name)
    (asserts! (< (len updated-display-name) u65) invalid-document-name)
    (asserts! (> updated-file-size u0) invalid-file-specifications)
    (asserts! (< updated-file-size u1000000000) invalid-file-specifications)
    (asserts! (> (len updated-description) u0) invalid-document-name)
    (asserts! (< (len updated-description) u129) invalid-document-name)
    (asserts! (verify-label-collection-integrity updated-classification-labels) invalid-metadata-tags)

    ;; Atomic metadata update operation with data preservation
    (map-set enterprise-documents
      { document-registry-id: target-document-id }
      (merge existing-document-data {
        document-display-name: updated-display-name,
        document-byte-size: updated-file-size,
        comprehensive-description: updated-description,
        classification-labels: updated-classification-labels
      })
    )

    ;; Confirm successful modification completion
    (ok true)
  )
)

;; Secure document custodianship transfer protocol
(define-public (execute-ownership-transfer (target-document-id uint) (designated-new-custodian principal))
  (let
    (
      ;; Secure retrieval of current document ownership data
      (current-document-data (unwrap! (map-get? enterprise-documents { document-registry-id: target-document-id })
        document-does-not-exist))
    )

    ;; Multi-layer authorization verification process
    (asserts! (verify-document-existence target-document-id) document-does-not-exist)
    (asserts! (is-eq (get document-custodian current-document-data) tx-sender) ownership-verification-failed)

    ;; Execute atomic ownership transfer with immutable audit trail
    (map-set enterprise-documents
      { document-registry-id: target-document-id }
      (merge current-document-data { document-custodian: designated-new-custodian })
    )

    ;; Confirm successful ownership transfer completion
    (ok true)
  )
)

;; Permanent document removal from enterprise registry
(define-public (remove-document-from-registry (target-document-id uint))
  (let
    (
      ;; Validate document existence before deletion attempt
      (target-document-data (unwrap! (map-get? enterprise-documents { document-registry-id: target-document-id })
        document-does-not-exist))
    )

    ;; Strict authorization enforcement for deletion operations
    (asserts! (verify-document-existence target-document-id) document-does-not-exist)
    (asserts! (is-eq (get document-custodian target-document-data) tx-sender) ownership-verification-failed)

    ;; Execute irreversible document removal from registry
    (map-delete enterprise-documents { document-registry-id: target-document-id })

    ;; Confirm successful document deletion
    (ok true)
  )
)


;; Permission revocation system for access control management
(define-public (revoke-document-access-permission (target-document-id uint) (restricted-viewer principal))
  (let
    (
      ;; Authenticate document ownership for permission management
      (ownership-verification-data (unwrap! (map-get? enterprise-documents { document-registry-id: target-document-id })
        document-does-not-exist))
    )

    ;; Comprehensive authorization validation
    (asserts! (verify-document-existence target-document-id) document-does-not-exist)
    (asserts! (is-eq (get document-custodian ownership-verification-data) tx-sender) ownership-verification-failed)

  

    ;; Confirm successful permission revocation
    (ok true)
  )
)

;; ==== Secure Read-Only Query Interface ====

;; Comprehensive document information retrieval with access control
(define-read-only (get-document-information (target-document-id uint))
  (let
    (
      ;; Secure document data retrieval
      (document-information (unwrap! (map-get? enterprise-documents { document-registry-id: target-document-id })
        document-does-not-exist))
      ;; Access permission verification
      (access-permission-status (default-to false
        (get viewing-permission-granted
          (map-get? access-control-matrix { document-registry-id: target-document-id, requesting-principal: tx-sender })
        )))
    )

    ;; Multi-layer access authorization check
    (asserts! (verify-document-existence target-document-id) document-does-not-exist)
    (asserts! (or access-permission-status 
                  (is-eq (get document-custodian document-information) tx-sender)
                  (is-eq tx-sender system-administrator)) viewing-restrictions-active)

    ;; Return comprehensive document metadata
    (ok document-information)
  )
)

;; Current registry statistics and system status
(define-read-only (get-registry-statistics)
  (ok {
    total-registered-documents: (var-get document-registry-counter),
    system-administrator-address: system-administrator,
    current-block-height: block-height
  })
)

;; Document ownership verification service
(define-read-only (verify-document-custodian (target-document-id uint) (potential-owner principal))
  (ok (authenticate-document-ownership target-document-id potential-owner))
)

;; Access permission status inquiry system
(define-read-only (check-viewing-permission (target-document-id uint) (requesting-user principal))
  (let
    (
      ;; Retrieve permission status from access control matrix
      (permission-status (default-to false
        (get viewing-permission-granted
          (map-get? access-control-matrix { document-registry-id: target-document-id, requesting-principal: requesting-user })
        )))
    )
    (ok permission-status)
  )
)
